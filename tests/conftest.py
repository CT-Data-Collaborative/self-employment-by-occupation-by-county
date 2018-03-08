import pytest
import datapackage



@pytest.fixture
def counties():
    """Load our county list datapackage"""
    dp = datapackage.DataPackage('https://raw.githubusercontent.com/scuerda/ct-county-list/master/datapackage.json')
    return dp.resources[0].data


