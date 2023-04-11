create table if not exists dim_tempo(
	id serial not null primary key,
	ano varchar(100)
);

create table if not exists dim_local(	
	id serial not null primary key,
	regiao varchar(100)
);

create table if not exists dim_individuo(	
	id serial not null primary key,
	id_questionario integer not null unique,
	sexo varchar(100),
	idade varchar(100),
	faixa_etaria varchar(100),
	grau_instrucao varchar(100),
	pea varchar (100),
	renda varchar(100),
	habilidade_programacao varchar(100)
);

create table if not exists dim_dispositivo(
	id serial not null primary key,
	uso_internet_computador varchar(50),
	uso_internet_notebook varchar(50),
	uso_internet_tablet varchar(50),
	uso_internet_celular varchar(50),
	uso_internet_video_game varchar(50),
	uso_internet_tv varchar(50),
	uso_internet_outro varchar(50)
);

create table if not exists dim_local_uso(
	id serial not null primary key,
	uso_casa varchar(100),
	uso_trabalho varchar(100),
	uso_escola_ensino varchar(100),
	uso_casa_individuo_externo varchar(100),
	uso_centro_publico_gratis varchar(100),
	uso_centro_publico_pago varchar(100),
	uso_deslocamento varchar(100),
	uso_outros varchar(100),
	uso_frequencia varchar(100)
);

create table if not exists fato_uso_internet(
	id_local integer not null,
	id_local_uso integer not null,
	id_tempo integer not null,
	id_dispositivo integer not null,
	id_individuo integer not null,
	quantidade integer not null,
	frequencia_uso_email integer not null,
	frequencia_uso_rede_social integer not null,
	constraint fk_local foreign key(id_local) references dim_local(id),
	constraint fk_local_uso foreign key(id_local_uso) references dim_local_uso(id),
	constraint fk_tempo foreign key(id_tempo) references dim_tempo(id),
	constraint fk_dspositivo foreign key(id_dispositivo) references dim_dispositivo(id),
	constraint fk_inidividuo foreign key(id_individuo) references dim_individuo(id),
	PRIMARY key(id_local,id_local_uso,id_tempo,id_dispositivo,id_individuo)

);