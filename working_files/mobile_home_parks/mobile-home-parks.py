import pandas as pd

def get_full_address(str):
        split_string = str.split(',')
        last_element = split_string[-1] 
        arr_wo_phone = ''
        if ('(' in last_element):
            arr_wo_phone = split_string[:-1]
        else:
            arr_wo_phone = split_string
        address_only = ','.join(arr_wo_phone)
        return address_only

def get_zip(str):
    split_string = str.split(',') 
    zip = split_string[-1].strip().split(' ')[1]
    return zip

def get_state(str):
    split_array = str.split(',')
    state = split_array[-1].strip().split(' ')[0]
    return state

def get_city(str):
    split_string = str.split(',')
    city = split_string[-2].strip()
    return city

def get_address(str):
    split_string = str.split(',')
    address_arr = split_string[:-2]
    address = ' '.join(address_arr).strip()
    return address


num_cols = ['MH Spaces', 'RV Lots W/Drains', 'RV Lots W/O Drains']
mhp_dtypes = {
    'Park Name': str,
    'County': str,
    'Park Identifier': str,
    'Park Address': str,
    'Fire Authority': str,
    'Jurisdiction': str,
    'MH Spaces': 'Int64',
    'RV Lots W/Drains': 'Int64',
    'RV Lots W/O Drains': 'Int64',
    'Operated by': str
}

if __name__ == '__main__':
    # Import data
    file = 'cmir-mp-park-query-2022_02_22.xls'
    df = pd.read_excel(file, dtype=mhp_dtypes)

    # Rename columns
    df = df.rename(columns={
        'Park Name': 'ParkName',
        'Park Identifier': 'ParkIdentifier',
        'Park Address': 'ParkAddress',
        'Fire Authority': 'FireAuthority',
        'MH Spaces': 'MH_Spaces',
        'RV Lots W/Drains': 'RV_Lots_W_Drains',
        'RV Lots W/O Drains': 'RV_Lots_WO_Drains',
        'Operated by': 'OperatedBy'
    })
    
    # Create new column and copy address (without phone number over to new column)
    df['FullAddress'] = df['ParkAddress'].apply(lambda x: get_full_address(x))

    # Manually set some addresses for mobile home parks that are otherwise geocoded incorrectly
    # These addresses were manually verified using Google Maps 3/8/22
    df.loc[df['ParkIdentifier'] == '14-0001-MP', 'FullAddress'] = '150 Tinemaha Rd #106, Independence, CA 93526'.upper() # Aberdeen Resort in Inyo County, original address looks like a PO box or similar
    df.loc[df['ParkIdentifier'] == '33-0486-MP', 'FullAddress'] = '47340 Jefferson St, Indio, CA 92201'.upper() # Indian Wells RV Resort, original address was entered or transcribed incorrectly with a space between address numbers
    df.loc[df['ParkIdentifier'] == '33-0561-MP', 'FullAddress'] = '51374 Tyler St, Coachella, CA 92236'.upper() # Palmera Estates MHP/Las Palmeras Housing Associates. Original address was entered or transcribed incorrectly with a space between address numbers
    df.loc[df['ParkIdentifier'] == '37-0231-MP', 'FullAddress'] = '11670 Sunrise Hwy, Mt Laguna, CA 91948'.upper() # Al Bahr Shrine Camp. Original address was a description, not an actual address
    
    # Delete this record . I cannot verify the park or the address. There is another King Island park in the dataset, but it is in Stockton, not Byron. I think that one is the correct address. Department of Homeland Security data does NOT have this park.
    df.drop(df[df['ParkIdentifier'] == '39-0189-MP'].index, inplace=True)

    # Break up the address field into individual elements
    df['Address'] = df['FullAddress'].apply(lambda x: get_address(x))
    df['City'] = df['FullAddress'].apply(lambda x: get_city(x))
    df['State'] = df['FullAddress'].apply(lambda x: get_state(x))
    df['Zip'] = df['FullAddress'].apply(lambda x: get_zip(x))

    # Write CSV
    df.to_csv('mobile-home-parks.csv', index=False)


