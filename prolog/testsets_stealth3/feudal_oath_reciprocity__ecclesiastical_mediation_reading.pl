% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Sacramentally Bound Feudal Oath under Ecclesiastical Mediation (Ecclesiastical Mediation Reading)
 *   domain: economic/political/legal-historical
 *
 * SUMMARY:
 *   From the ninth-century Carolingian collapse onward, armed elites in Latin
 *   Christendom held together by sworn oath: homage and fealty exchanged land
 *   and protection for service, with the oath's binding force grounded in its
 *   sacramental character — God witnessed it, perjury endangered the soul,
 *   and the church held the machinery of sanction and absolution. On the
 *   ecclesiastical mediation reading instantiated here, this arrangement
 *   genuinely coordinated a stateless warrior order AND asymmetrically
 *   extracted: the church converted its adjudicatory monopoly over oath
 *   compliance into interpretive authority, tithe streams, court fees, and
 *   leverage over princes, while lords accepted capped extraction and
 *   tenantry continued paying dues the same preaching sanctified. KEY AGENTS
 *   (by structural relationship): - ecclesiastical_hierarchy: Agenda-setting
 *   enforcer (institutional/arbitrage) — administers the sanction machinery,
 *   adjudicates disputes, collects tithes and fees - secular_lordship:
 *   Constrained payer (powerful/constrained) — extraction capped by
 *   penitential discipline, pays tithes, concedes jurisdiction -
 *   fief_holding_vassals: Protected beneficiary (organized/constrained) —
 *   tenure claims made enforceable beyond either party's momentary advantage
 *   - anointed_monarchs: Dual-positioned payer-beneficiary
 *   (institutional/identity_locked) — purchases sacral legitimacy with
 *   submission to ecclesial correction - manorial_tenantry: Residual payer
 *   (powerless/trapped) — dues sacralized by the same frame that moderates
 *   them - women_and_landless_laborers: Excluded seat (powerless/trapped) —
 *   labor supports the order, no oath-standing - heretical_dissenters:
 *   Excluded repudiators (powerless/trapped) — reject the sacramental kernel
 *   wholesale, answered by crusade and inquisition - legal_historians:
 *   Analytical observer (analytical/analytical). Claim and metrics are
 *   authored independently: claimed_type tangled_rope states this reading's
 *   structural belief (real coordination, real asymmetry, active
 *   enforcement); the metric values state descriptively what the
 *   arrangement's operation looked like across the interval. The engine
 *   computes each seat's type from the structural data; divergence between
 *   claim and computed seat-types is data, not error.
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: agenda_setter + beneficiary (institutional/arbitrage) — runs penance, excommunication, interdict, and oath-dispute adjudication; collects tithes, court fees, and interpretive authority
 *   - secular_lordship: payer + beneficiary (powerful/constrained) — holds legitimated dominion; extraction capped by penitential discipline; pays tithes and jurisdictional concessions
 *   - fief_holding_vassals: beneficiary (organized/constrained) — swears homage on relics; receives enforceable tenure claims and bounded lordly demands
 *   - anointed_monarchs: payer + beneficiary (institutional/identity_locked) — crowned and corrected by the church; gains sacral legitimacy and clerical administration; identity constituted by the anointing
 *   - manorial_tenantry: payer + beneficiary (powerless/trapped) — owes week-work, boons, and dues the frame sanctifies; receives peace-of-God protection and famine remissions
 *   - women_and_landless_laborers: excluded (powerless/trapped) — perform supporting labor without oath-standing; protection allocates through a mechanism they cannot enter
 *   - heretical_dissenters: excluded (powerless/trapped) — refuse oaths or the sacramental frame; suppressed by enforcement machinery rather than debated
 *   - legal_historians: observer (analytical/analytical) — reconstruct the whole structure from cartularies, polyptychs, conciliar acta, and registers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Sacramentally Bound Feudal Oath under Ecclesiastical Mediation (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "economic/political/legal-historical").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'f2fc7332-be44-4e6e-b6ed-05c8d67a7de9').
narrative_ontology:cs_kernel_codification('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', formalized).
narrative_ontology:cs_authority_grounding('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', lineage).
narrative_ontology:cs_interpretation_layer_present('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9').
narrative_ontology:cs_reading_relation('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', feudal_oath_reciprocity__lord_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', foundational, oath_binding_is_sacramental).
narrative_ontology:cs_axiom_status(oath_binding_is_sacramental, holdable).
narrative_ontology:cs_axiom_grounding('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', oath_binding_is_sacramental, theological).
narrative_ontology:cs_axiom('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', foundational, caritas_bounds_secular_extraction).
narrative_ontology:cs_axiom_status(caritas_bounds_secular_extraction, holdable).
narrative_ontology:cs_axiom_grounding('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', caritas_bounds_secular_extraction, theological).
narrative_ontology:cs_axiom('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', secondary, oath_disputes_adjudicated_ecclesiastically).
narrative_ontology:cs_axiom_status(oath_disputes_adjudicated_ecclesiastically, holdable).
narrative_ontology:cs_axiom_grounding('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', oath_disputes_adjudicated_ecclesiastically, conventional).
narrative_ontology:cs_reference_frame('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', sacramental_covenant_order).
narrative_ontology:cs_drift_state('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', late_medieval_royal_law_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2fc7332-be44-4e6e-b6ed-05c8d67a7de9', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, fief_holding_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lordship).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, manorial_tenantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lordship).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, anointed_monarchs).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, manorial_tenantry).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, anointed_monarchs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sacramental machinery on which oath-binding rests: consecrates kings, presides at homage ceremonies, hears confessions, declares excommunications and interdicts, adjudicates oath disputes through councils and canonist courts. Collects tithes from every estate, fees for dispensations and probates, and first-fruits; its sanction is the credible threat that makes promises bind. When princes resist, it arbitrages among them — shifting legatine support, coronation cooperation, and favor to rivals.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, beneficiary).

% Holds landed dominion legitimated by consecration and sworn fidelity. Owes military retinue, hospitality, and justice to superiors; owes tithes, first-fruits, and jurisdictional concessions to the church. Its freedom to press tenants for surplus is capped by penitential discipline — extraction framed as rapacity draws confession, penance, and ultimately excommunication. In exchange it receives the legitimation that keeps its own vassals bound and its successions recognized. Leaving the frame means forfeiting both the legitimation and the salvation-assurance its household operates inside.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lordship, payer,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lordship, beneficiary).

% Swears homage and fealty on relics; receives a fief, protection, and standing in the lord's court. Its tenure claim is enforceable because the oath binds beyond either party's momentary advantage — a dispossessed vassal can appeal to adjudication rather than to arms alone. Its own counter-obligations are finite and specified, capping what its lord can demand. Renouncing fealty forfeits the fief; staying inside the frame is the condition of holding anything at all.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, fief_holding_vassals, beneficiary,
    organized, generational, constrained, regional).

% Receives crown and anointing through ecclesiastical ceremony and swears coronation oaths to justice, protection of churches, and hereditary right. Gains sacral legitimacy, clerical literacy and record-keeping, and a sanction usable against rebellious magnates. Pays for this in submission to correction: popes have suspended kings, laid interdicts on kingdoms, and dictated penances. The monarchy's identity is constituted by the anointing it submits to — ruling without the frame is, within the frame's own terms, not ruling at all.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, anointed_monarchs, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, anointed_monarchs, beneficiary).

% Owes week-work, boon works, and dues in kind and cash on the lord's demesne; marriages, transfers, and milling pass through seigneurial tolls. The same preaching that caps lordly rapacity instructs tenants to render service as owed to God — the frame sanctifies the obligations it moderates. Receives the Peace-of-God ceiling on knightly violence and occasional famine-year remissions. Cannot legally leave the land; improving exit arrived late and piecemeal through flight to franchised towns and communal charters.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, manorial_tenantry, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, manorial_tenantry, beneficiary).

% Perform the field labor and domestic production on which the entire structure rests, but hold no oath-standing: homage and fealty forms exclude them, protection allocates through the sworn relationships they cannot enter, and dispute adjudication proceeds among parties who are not them. Their objection — that security distributed by swearing capacity is not security for them — has no forum inside the arrangement.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, women_and_landless_laborers, excluded,
    powerless, immediate, trapped, local).

% Communities in southern France, Lombardy, and the Rhineland that reject the sacramental frame itself — some refusing to swear any oath at all, others denying the sacraments' validity. Their existence demonstrates the frame's reach was not universal. The enforcement machinery answers them with crusade and inquisition rather than doctrinal debate; exit from the frame's territory means exile or death.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, heretical_dissenters, excluded,
    powerless, biographical, trapped, regional).

% Reconstruct the arrangement's operation from cartularies, polyptychs, conciliar acta, penitentials, and royal registers across the whole span; read each estate's records against the others and see simultaneously what each participant seat could only experience locally.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, legal_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts personal armed rivalry among a warrior elite into durable, cross-generational commitment where no state exists to enforce contracts: oath plus divine witness makes land-for-service, succession recognition, and feud restraint credible when breach is cheap and detection weaker; charity doctrine supplies a shared limit concept; ecclesiastical adjudication supplies a dispute-resolution channel.
% TRANSFER_FUNCTION: Moves labor services and dues from manorial tenantry upward to lords; military service from vassals to lords; tithes, court fees, dispensation and probate revenues, and adjudicatory authority from all estates to the ecclesiastical hierarchy; protection, enforceable tenure, and sacral legitimation downward to vassals and monarchs respectively.
% ABSENT_VOICES: Those without oath-standing: women, largely barred from the homage and fealty forms; the unfree below tenant standing; and hired laborers — their labor funds the order, but the mechanism allocating protection keys on a swearing capacity they lack. Heretical dissenters object at the root that binding social order through sacrament is itself the corruption; they were answered by crusade and inquisition rather than argument. All three voices sit outside the adjudicated conversation, which took place among the three estates the arrangement provisioned.
% DISAPPEARANCE_RATIONALE: Fief tenure claims lose binding force overnight — possession reverts to whoever can hold it by force; succession disputes reignite across every dynasty; the church loses adjudicatory traffic, tithe legitimacy, and leverage over princes; tenantry lose the peace-of-God ceiling on elite violence. The historical record of what happened where the frame weakened — the castellan brigandage epochs of the tenth and eleventh centuries — previews the rearrangement: armed competition until some new commitment technology, royal courts, written registers, wage contracts, reconstitutes order decades later.
% FOUNDING_PROBLEM: After Carolingian public authority fragmented from the ninth century onward: how do you get armed men to honor long-term commitments — land for service, succession, restraint in feud — when there is no state to enforce agreements and breach is cheap?
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary estate by modern institutional historiography (Duby on the mutation of the year 1000, Strayer on medieval state formation, Wickham on the post-Roman fragmentations), which reconstructs the commitment problem from charters and annals without any stake in the medieval rents; by Byzantine and Islamic diplomatic correspondence recording Latin political fragmentation as a real condition; and by royal capitularies — authored by state, not church, hands — documenting feud and oath-breaking as governance failures. Susan Reynolds' Fiefs and Vassals additionally attests, from outside all three medieval estates, that the coherent unified-system framing is itself contested, corroborating that the kernel's content, not merely its evaluation, was disputed. Contemporaneously the problem was live through most of the interval; by 1300 royal courts and written charters offered working substitutes and the parties disputed whether the sacramental mechanism remained necessary — hence contested rather than dead.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 (equal to the T=1300 measurement): the arrangement truly bounded lordly predation — castellan violence was repeatedly checked by penitential and interdict pressure — while routing tithes, probate and dispensation fees, adjudicatory traffic, and coronation prerogatives to the church, and while leaving manorial dues intact beneath a sanctifying gloss. Moderate, asymmetric, real on both sides: textbook tangled-rope shape. Suppression 0.55 is a raw structural property the engine does not scale: the enforcement backbone is spiritual coercion (confession discipline, excommunication, interdict) backed where convenient by temporal arms. Theater 0.38: the charity-limit and dispute-resolution functions were substantially real through most of the interval, but by 1300 a growing share of activity is performative maintenance — ceremonial oath renewals, jurisdictional claims pressed by an apparatus whose real work royal courts were absorbing. Accessibility collapse 0.45: alternatives existed and grew (royal justice, communal self-government, charter-and-register contracting, flight to liberty towns), but exiting mid-interval meant forfeiting fief, legitimation, or salvation-assurance, so collapse was partial, not complete. Resistance 0.55: sustained and organized — the imperial investiture wars, baronial resistance to clerical taxation, communal exemption charters. The suppression_requirement series is authored deliberately notwithstanding the static-picture caution, because enforcement-capacity change IS this story's traced dynamic: machinery built up across the Peace of God councils, the Gregorian reform, and the twelfth-century canon-law consolidation, peaked around 1200 with the full decretal-interdict system, then visibly eroded as royal courts substituted and papal coercive capacity failed publicly (Anagni, 1303) — a rise-then-fall arc, not a monotone ratchet. All three series share one six-point grid (900–1300) per the alignment rule, and final values equal the base_properties scalars. Suppression here is structural — institutional sanction machinery — not primarily internalized; penitential interiority was real but the binding force was the institution standing behind it.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience structurally different arrangements. From the church seat this is a divinely ordered charity-limitation it administers at real administrative cost; from the magnate seat it is confiscatory adjudication plus tithe drain dressed as pastoral care; from the vassal seat it is the only thing making a fief claim worth the paper of no paper at all — enforceable rights where previously only force decided; from the manorial seat it is subordination with a sanctifying gloss and an occasional famine-year remission. The engine computes these as divergent per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates among them. Identity-lock dynamics concentrate in the monarch seat: anointing constitutes kingship rather than merely endorsing it — a king who exits the frame ceases, within the frame's own terms, to be a king — so effective extraction is amplified for that seat beyond what its material position alone would predict; the classification would change materially if that fusion broke, which is precisely what later princely sacralization outside Roman jurisdiction demonstrated. Coalition potential among the powerless seats was real and materialized late in the interval as rural flight to franchised towns and communal charters — an improving exit that contributed to the enforcement-decay tail of the suppression series.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The ecclesiastical hierarchy sits near the beneficiary pole: the constraint subsidizes it (interpretive rents, tithes, fees flow in; enforcement costs are modest against receipts), and its arbitrage-grade positional mobility (shifting favor among rival princes) keeps it far from any target position. Fief-holding vassals sit beneficiary-ward: enforceable tenure claims and bounded lordly demands outweigh the finite obligations they discharge. Secular lordship sits target-ward but not maximally: capped extraction, tithe payments, and conceded jurisdiction are genuine costs, pulled partway back toward symmetry by the legitimation receipts that keep their own inferiors bound. Manorial tenantry sit near the target end and, being trapped, take the amplified effective extraction — their moderation benefit is a partial offset on a large residual dues burden. Anointed monarchs occupy a mixed position the beneficiary/victim arrays deliberately do not flatten: their dual seat (payer of obedience, beneficiary of sacral legitimacy) plus identity lock places them between poles with amplification from the lock. The excluded seats — women and landless laborers, heretical dissenters — sit outside the arrangement's accounting entirely: no declared position feeds their directionality, and that omission is itself the exclusion the absent-voices answer records.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both mislabelings. Read through the lord-extraction lens alone, the arrangement looks like pure predation with a pious mask; naming the coordination function — credible cross-generational commitment under statelessness, where breach was otherwise cheap and detection weaker still — blocks that error, because abolishing the frame would not have freed dependents, it would have removed the only binding force lords confronted, returning decision to armed contest. Read as pure coordination, the ledger loses the interpretive rents, the sacralized dues, and the jurisdictional concessions; the victim declarations block that error. On mandatrophy: the founding problem (commitment technology under absent public authority) remained live through nearly all of the interval, so the arrangement is not mandatrophy-resolved within this window; the founding-problem interview records status contested at the close, as royal courts and written registers offered substitutes and contemporaries disputed the sacramental mechanism's continuing necessity. The rising theater_ratio tail is the leading indicator of the obsolescence that a longer-interval successor story would confirm or refute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the ecclesiastical_mediation_reading of the feudal_oath_reciprocity kernel; would classification survive under the sibling readings (lord_extraction_reading, vassal_coordination_reading), or does the disagreement locate in the binding mechanism itself?',
    'Comparative classification across the three sibling stories sharing the fixed referent (the standing oath-mediated arrangement): convergence on tangled_rope indicates the structure dominates the readings; divergence maps the disagreement onto the binding-mechanism axis (sacrament versus force versus charter text).',
    'Under lord_extraction_reading epsilon rises toward snare territory with lordly seats as agenda-setters; under vassal_coordination_reading epsilon falls toward rope with enforcement migrated to written instruments; the ecclesiastical reading''s distinctive structural claim is that the mediating seat captures interpretive rents while performing the limiting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame routing: which reading of the oath-kernel governs classification; siblings are separate files, not hedges inside this one.').

omega_variable(
    genuine_limit_vs_sanctified_continuity,
    'Did the ecclesiastical frame actually reduce net extraction borne by dependent populations, or did it relabel existing extraction while collecting its own rents on top of it?',
    'Compare manorial burden trajectories across regions entering the sacramental frame at different times (Saxon lands after Frankish incorporation, Scandinavian lands after conversion, Slavic marcher zones) using polyptychs and estate surveys; a dues step-change correlated with conversion timing would indicate relabeling rather than reduction.',
    'If relabeling dominated, epsilon is understated and the type trends toward snare with the church seat as capturer; if genuine limitation dominated, the coordination share is larger and rope elements strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_limit_vs_sanctified_continuity, empirical, 'Whether charity-limitation subtracted from total extraction or merely rebranded it while adding ecclesial rents.').

omega_variable(
    enforcement_decay_trajectory,
    'Is the late-interval decline in enforcement capacity (falling interdict efficacy after 1250, royal courts absorbing oath-dispute adjudication) terminal dissolution or a trough preceding revival?',
    'Track post-1300 incidence in episcopal registers (excommunication and absolution counts), conciliar legislative output, and royal-court citation volumes displacing church courts.',
    'Terminal decay dates a transition toward inertial persistence with the real function migrated elsewhere and performance remaining; a revival resets suppression_requirement upward and sustains the tangled_rope classification indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Whether the enforcement build-up-then-erosion arc terminates the arrangement or pauses it.').

omega_variable(
    kernel_codification_framing,
    'Is the kernel canon-law-formalized (Gratian and the decretals specify the obligation) or practice-distributed (customary oath usage preceding and exceeding codification)?',
    'Test whether pre-Gratian oath practice already exhibited the full obligation structure the codification claims to define; if the formalization codified rather than created the norm, the distributed framing is defensible and yields a different commitment-system pattern.',
    'The formalized framing yields a centralized adjudicatory pattern matching this reading''s agenda-setter concentration; the distributed framing diffuses authority across practitioner communities and shifts the authority-grounding inputs toward practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'CS-framing under-determination: formalized-canon-law kernel versus distributed customary-practice kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 900, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t900, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 900, 0.14).
narrative_ontology:measurement(feud_tr_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 950, 0.16).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.21).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1100, 0.25).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1200, 0.3).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.38).

% Extraction over time
narrative_ontology:measurement(feud_be_t900, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 900, 0.42).
narrative_ontology:measurement(feud_be_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 950, 0.48).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.55).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1100, 0.6).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t900, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 900, 0.35).
narrative_ontology:measurement(feud_su_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 950, 0.42).
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.52).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1100, 0.64).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the feudal oath' conflates three structurally distinct readings of one kernel (feudal_oath_reciprocity): this ecclesiastical-mediation reading (binding runs through sacrament; the mediating church captures interpretive authority; lords are constrained), the lord-extraction reading (the oath licenses extraction bounded only by service capacity), and the vassal-coordination reading (fixed reciprocal obligations enforced by charter text). Each is authored as a separate constraint story with its own epsilon over the shared referent — the standing oath-mediated arrangement — per the epsilon-invariance principle; the epsilon differences across readings are reading-indexed values over a fixed referent, not measurement noise. Family members are linked through affects_constraints in all three files. The upstream/downstream structure runs: ecclesiastical mediation shaped the legitimacy conditions under which charter-text enforcement later operated (charters initially borrowed sacral force by being sworn on relics, then progressively stripped it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
