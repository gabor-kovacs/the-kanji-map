import React, { useState, useEffect } from 'react';

import { BrowserRouter as Router, Route, Switch } from 'react-router-dom';

import App from './App';
import About from './About';
import Kanjis from './Kanjis';
import Header from './Header';

export default function Main() {
	// loading chise decomposition from JSON
	const [chise, setChise] = useState(null);
	// loading data from JSON
	const [data, setData] = useState(null);
	// LOAD COMPOSTION AND DATA JSON
	useEffect(() => {
		Promise.all([fetch('/data/composition.json'), fetch('/data/data.json')])
			.then(async ([compRes, dataRes]) => {
				const compJson = await compRes.json();
				const dataJson = await dataRes.json();
				setChise(compJson);
				setData(dataJson);
			})
			.catch((error) => {
				console.log('error=' + error);
			});
	}, []);

	return (
		<>
			<Router>
				<Header />
				<Switch>
					<Route exact path="/">
						<App chise={chise} data={data} />
					</Route>
					<Route path="/about">
						<About />
					</Route>
					<Route path="/kanji">
						<Kanjis data={data} />
					</Route>
				</Switch>
			</Router>
		</>
	);
}
