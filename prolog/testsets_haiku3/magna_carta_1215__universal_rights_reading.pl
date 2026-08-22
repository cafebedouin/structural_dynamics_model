% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Universal Due Process Right (1215 Reading)
 *   domain: constitutional/political/legal
 *
 * SUMMARY:
 *   This constraint story instantiates the UNIVERSAL RIGHTS READING of Magna
 *   Carta (kernel: magna_carta_1215). Under this reading, Clause 39 ('No free
 *   man shall be arrested, imprisoned, disseised [dispossessed], outlawed,
 *   exiled, or in any way ruined, nor will we go against him or send against
 *   him, except by the lawful judgment of his peers or by the law of the
 *   land') is interpreted as establishing a transhistorical principle: all
 *   persons subject to state power hold a right against arbitrary detention,
 *   extrajudicial punishment, and denial of due process. The text's 'free
 *   men' is read to encompass all inhabitants eventually, not merely the
 *   baronial class of 1215. Under this reading, the constraint's beneficiary
 *   set is universal (all persons subject to the sovereign), and the
 *   constraint coordinates a fundamental rule of law principle: no agent of
 *   the state can act against any person without legal justification and
 *   orderly procedure. This is distinct from the baronial_privilege_reading
 *   (Clause 39 as feudal contract binding only the contracting parties —
 *   barons and Crown) and from the living_document_reading (Clause 39 as
 *   adaptive constitutional substrate whose meaning is constituted by
 *   interpretive tradition rather than fixed in 1215). The universal reading
 *   claims that Clause 39 functions AS WRITTEN to protect all persons; it
 *   does not require centuries of reinterpretation to activate its scope, nor
 *   is its reach limited to a feudal elite.
 *
 * KEY AGENTS:
 *   - all_persons_subject_to_state: beneficiary (universal; protected against arbitrary state action)
 *   - crown_and_state_agents: agenda_setter and implicit payer (enforce the constraint, accept limitations on prerogative)
 *   - judicial_apparatus: agenda_setter and observer (interprets and applies the constraint)
 *   - landowning_barons_1215_class: secondary beneficiary and observer (original contracting parties; see reading_relations note below)
 *   - interpretive_tradition_scholarly_community: observer and secondary agenda_setter (maintains and develops the reading over centuries)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.28).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.41).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Universal Due Process Right (1215 Reading)").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional/political/legal").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, 'd7cb17d8-98a0-48fe-91e6-b961327bc3d5').
narrative_ontology:cs_kernel_codification('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', fixed_text).
narrative_ontology:cs_authority_grounding('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', lineage).
narrative_ontology:cs_interpretation_layer_present('d7cb17d8-98a0-48fe-91e6-b961327bc3d5').
narrative_ontology:cs_reading_relation('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', foundational, clause_39_universal_scope).
narrative_ontology:cs_axiom_status(clause_39_universal_scope, holdable).
narrative_ontology:cs_axiom_grounding('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', clause_39_universal_scope, deontological).
narrative_ontology:cs_axiom('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', foundational, rule_of_law_principle_1215_enacted).
narrative_ontology:cs_axiom_status(rule_of_law_principle_1215_enacted, holdable).
narrative_ontology:cs_axiom_grounding('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', rule_of_law_principle_1215_enacted, deontological).
narrative_ontology:cs_reference_frame('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', clause_39_universal_due_process_principle).
narrative_ontology:cs_drift_state('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', contemporary_human_rights_consensus_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d7cb17d8-98a0-48fe-91e6-b961327bc3d5', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons_subject_to_state).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, landowning_barons_1215_class).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, universal_human_rights_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, equal_protection_principle).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, due_process_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a claim against arbitrary detention, extrajudicial punishment, and denial of due process. Their protection depends on effective legal mechanism (courts, procedure, remedy). Exit from the constraint means fleeing the jurisdiction; accepting state authority means invoking the constraint. They benefit equally regardless of wealth, land, or status — the constraint is their equalizer against state power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons_subject_to_state, beneficiary,
    powerless, biographical, trapped, universal).

% Enforce and administer the constraint through legal procedure, courts, and oversight. The constraint limits their prerogative to act arbitrarily; they must justify state action by law and process. They collect the legitimacy benefit of rule of law (stable governance, public acceptance) but pay the cost of procedural obligation and judicial review. They can reform the constraint by changing law, but cannot unilaterally abandon it without losing legitimacy.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, crown_and_state_agents, agenda_setter,
    institutional, generational, mobile, universal).

% Interprets Clause 39 and applies it to specific cases; guards against arbitrary state action by subjecting state exercises of power to legal review. They sit at the enforcement point: their decisions determine whether a detention is lawful or arbitrary, whether a punishment conforms to process or violates the clause. They are constrained by the text and precedent but exercise interpretive power over its scope and application.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judicial_apparatus, agenda_setter,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, judicial_apparatus, observer).

% The original contracting parties (1215); they negotiated Clause 39 to protect their property and persons against arbitrary Crown seizure. In the baronial reading, they are the primary beneficiaries; in the universal reading, they are a subset of the universal beneficiary set, no longer privileged. They hold historical standing to invoke the clause (it was their charter) but no special procedural advantage in the universal reading. Their exit involves legal challenge to Crown authority or civil war — costly, but historically exercised.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, landowning_barons_1215_class, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, landowning_barons_1215_class, observer).

% Scholars, judges, and jurists who interpret Clause 39 across centuries, gradually extending its scope from baronial privilege to universal right. They do not directly enforce the constraint but shape how courts understand and apply it. Their interpretations become embedded in precedent, influencing the constraint's practical operation. In the living-document reading, their tradition IS the source of meaning; in the universal reading, they are commentators on a fixed text.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, interpretive_tradition_scholarly_community, observer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, interpretive_tradition_scholarly_community, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the principle that all state power over persons must be exercised through law and orderly procedure, not arbitrary will. Coordinates judicial review as the mechanism for enforcing this principle: courts stand between the state apparatus and individuals, adjudicating whether state action conforms to law. This solves the coordination problem of restraining concentrated power — without the principle and its enforcement mechanism, state agents face incentives to act arbitrarily; with it, they must justify action by law and defend it in court.
% TRANSFER_FUNCTION: The constraint moves procedural accountability: from a regime where state agents can act unilaterally, to one where they must justify action to courts and citizens. It transfers the power to determine legality from state agents alone to a shared set of judges, law, and precedent. It does not transfer wealth but reshapes the distribution of authority — from concentrated (Crown alone) to distributed (law, courts, and the procedures that govern both). Citizens transfer their obedience from obedience-to-will to obedience-to-law.
% ABSENT_VOICES: Absolute monarchs and agents of despotic regimes would object — they would argue that unconstrained executive power is necessary for effective governance, rapid response to crises, and maintenance of order. Authoritarian voices, historically and contemporaneously, argue that procedural constraint on state power undermines state capacity. These parties are structurally excluded from the constraint's legitimation narrative in democracies that accept it, but they remain active agents in contexts where the constraint is rejected or weakly enforced.
% DISAPPEARANCE_RATIONALE: If Clause 39 and its principle vanished overnight, state actors would regain unconstrained authority to detain, punish, and exile without legal justification or judicial review. Citizens would lose the mechanism by which they can challenge arbitrary state action. Courts would lose their authority to review state exercises of power. The constitutional structure of democracies depends on this constraint; its removal would require replacing the entire framework of rule of law and judicial oversight with some alternative mechanism of state restraint — or accepting despotic governance.
% FOUNDING_PROBLEM: Medieval Crown prerogative permitted arbitrary arrest, imprisonment, dispossession, exile, and execution without legal justification or opportunity for the subject to defend themselves. Barons and eventually subjects were vulnerable to the Crown's will; no orderly process stood between the Crown's decision to act and the action's execution. The charter was negotiated to establish that the Crown could not act thus — that law and procedure would bind even the sovereign.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary human rights organizations (Amnesty International, Human Rights Watch) attest that arbitrary detention and extrajudicial punishment remain widespread globally; millions of persons lack effective due process protection. Comparative constitutional scholarship (Tamanaha, Peerenboom, Waldron on rule of law) attests that Clause 39's principle — binding state power to law and procedure — remains contested and incompletely realized. Historical record (parliamentary statutes reaffirming Magna Carta repeatedly; English Civil War invocation of the charter against Crown prerogative; American and French revolutionary appropriation of its principles) corroborates that the founding problem persisted long after 1215. Outside the beneficiary set (all_persons_subject_to_state), independent observers confirm: the constraint was negotiated because the problem was real, and the problem remains unsolved in many contexts.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is MODEST (0.28 at interval end), not negligible, because the constraint imposes genuine coordination cost on state apparatus — legal process, procedural safeguards, and judicial review infrastructure are real expenditures. However, extractiveness is LOW, not HIGH, because the constraint does not transfer wealth or power from identifiable beneficiaries to identifiable payers; all persons gain the protection equally. The constraint coordinates a SHARED GOOD (rule of law, procedural legitimacy) rather than a TRANSFER. Suppression measurement (0.41 at interval end) tracks the enforcement intensity required to maintain the constraint against contrary state incentives — a state agent facing incentives to act arbitrarily must be suppressed (via law, oversight, punishment for violation) to comply. Suppression DECLINES over the interval (0.72 to 0.41) because the constraint's internalization in Western legal culture increases: violation becomes unthinkable, not merely punished. Theater_ratio DECLINES (0.45 to 0.18) because the constraint's performative aspect (ceremonial reaffirmation of the principle) shrinks relative to its functional aspect (actual legal process operating); the constraint becomes less theater and more infrastructure. The time interval (0–320) spans ~1215–1535, marking the early consolidation and reaffirmation phase of Magna Carta in English legal practice. The interval endpoint (320 ≈ 1535) captures the constraint at a moment when reaffirmations (Confirmatio Cartae, parliamentary statutes) have established the principle as durable law, not a negotiated exception.
 *
 * PERSPECTIVAL GAP:
 *   Under the universal reading, there is NO perspectival gap: all seats (state agents, subjects, judges) are bound by the same constraint and benefit from the same principle. However, CONTEXTUAL ASYMMETRY exists: state agents experience the constraint as a LIMITATION ON PREROGATIVE (they pay via procedural obligation); subjects experience it as a PROTECTION (they benefit). The engine may compute different directionalities per seat despite the shared principle — state agents' directionality approaches the target end (d near 1.0: constrained, powerful, bounded by the rule) while subjects approach beneficiary (d near 0.0: protected, access to remedy). This divergence arises not from contested beneficiary/victim designation but from the POWER ASYMMETRY in the relationship: the constraint is meaningful BECAUSE state agents hold power; subjects hold the constraint's beneficiary status only insofar as they can invoke it against that power.
 *
 * DIRECTIONALITY LOGIC:
 *   All persons subject to the state are beneficiaries (uniform designation: beneficiaries = all_persons_subject_to_state). There are no victims under this reading — the constraint protects rather than extracts. However, directionality computation will produce DIFFERENT d values for different stakeholder seats because directionality depends on POWER, TIME_HORIZON, and EXIT_OPTIONS, not on beneficiary/victim role alone. A powerless subject in a despotic regime has trapped exit and immediate time horizon, yielding d near 0.0 (beneficiary end) because escape is impossible and the constraint is their only recourse. A state agent (institutional power, generational time horizon, arbitrage exit via legal reform) would compute differently — possibly d > 0.5 (nearer target end) because the constraint binds them despite their power. The engine's per-seat computation captures this structural asymmetry without requiring us to split 'all_persons_subject_to_state' into micro-classes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE under this reading: arbitrary state power remains an active threat in jurisdictions without robust due-process enforcement. However, MANDATROPHY CANDIDATE STATUS exists in mature constitutional democracies where the constraint has been internalized to the point that violation has become inconceivable rather than merely illegal. The theater_ratio's decline (0.45 to 0.18) and suppression_requirement's decline (0.72 to 0.41) over the interval suggest a FUNCTIONAL SHIFT: early Magna Carta (1215–1300s) operates as ceremonial reaffirmation and political negotiation (high theater, high suppression needed to enforce against resistant Crown). By 1535, the constraint is becoming part of ordinary law — lower theater, suppression needed only at institutional margins. In the modern era (post-1700), full mandatrophy may apply in some jurisdictions: the constraint operates as standard legal procedure, with no surviving parties who benefit from or resist it — it has become constitutional bedrock. This reading does not resolve the mandatrophy question but documents how it emerges over time: a constraint born as political negotiation becomes law, then becomes infrastructure, then (possibly) becomes assumed background. The omega on suppression_internalization addresses whether this trajectory represents TRUE INTERNALIZATION (suppression becomes unnecessary because subjects and agents genuinely accept the principle) or SURVEILLANCE EVOLUTION (suppression is simply hidden in the assumption structure rather than explicit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_men_historical_referent,
    'Does ''free men'' in Clause 39 of Magna Carta (1215) denote only landowning barons and their retainers, or does it functionally extend to all persons subject to state power?',
    'Genealogical analysis of how subsequent courts and legislatures interpreted and applied Clause 39; examination of whether the principle was progressively broadened to encompass non-property-holding persons; historical record of who invoked it and on what grounds.',
    'If ''free men'' is read narrowly as baronial privilege only, this constraint dissolves into the baronial_privilege_reading and χ near-zero for non-landowners. If read universally, this reading holds and χ becomes modest positive (universal beneficiary, minimal extraction). The divergence is the pivot point between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_men_historical_referent, conceptual, 'Scope ambiguity in the original text''s referent class for ''free men''.').

omega_variable(
    clause_39_binding_force,
    'Does Clause 39''s prohibition on arbitrary detention and unjust punishment bind ONLY the contracting parties (barons + Crown), or does it establish a general principle applicable to all state agents and all persons?',
    'Comparative examination of how Clause 39 was cited in: (1) subsequent Magna Carta reissues and confirmations; (2) early common law cases extending or restricting its scope; (3) parliamentary debates and statutes claiming Magna Carta authority; (4) revolutionary-era invocations (English Civil War, American Revolution) and whether universal scope was explicitly claimed.',
    'The baronial reading reads Clause 39 as a bilateral feudal contract binding parties to their mutual obligations only. The universal reading reads it as a general principle of rule of law. This omega tracks which reading the evidentiary record supports — the relational structure of the constraint (who binds whom) depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clause_39_binding_force, empirical, 'Whether Clause 39 is a feudal bilateral obligation or a general legal principle.').

omega_variable(
    interpretive_tradition_legitimacy,
    'Does the centuries-long tradition of scholars, jurists, and legislatures interpreting Magna Carta as a universal due-process precedent constitute legitimate constitutional development (living-document reading), or does it represent a post-hoc distortion of the original text (competing readings)?',
    'Examination of the authority grounding for interpretive tradition: Is the tradition itself an authoritative source (lineage + practice authority), or merely a record of how various actors have misread the text? Did each wave of reinterpretation require fresh legitimation from the text, or did the tradition itself become self-perpetuating?',
    'If the interpretive tradition is itself authoritative, the living-document reading coexists with (or influences) the universal reading. If tradition is merely commentary without independent authority, then the universal reading must stand on the 1215 text alone. The authority grounding shapes what ''valid interpretation'' means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_tradition_legitimacy, conceptual, 'Whether interpretive tradition accumulation constitutes legitimate constitutional authority or post-hoc narrative construction.').

omega_variable(
    suppression_internalization,
    'Does the measured suppression (0.41) reflect structural barriers (legal prohibition of appeal, execution without trial, exile without process), or internalized acceptance of state power as legitimate by those it constrains?',
    'Historical examination of resistance patterns: Did subjects invoke Clause 39 when detained arbitrarily, or accept detention as prerogative? Were appeals to the clause strategically successful in preventing arbitrary punishment? Did the suppression persist after the legal framework changed, suggesting internalization?',
    'If suppression is primarily structural, the constraint''s effectiveness depends on enforcement infrastructure and can be changed by legal reform. If suppression is internalized, subjects carry it even after legal barriers are removed. The post-exit trajectory of suppression indicates which mechanism dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression of arbitrary state power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 0, 320).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(magn_tr_t0, projected).
narrative_ontology:measurement(magn_tr_t80, magna_carta_1215__universal_rights_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(magn_tr_t80, observed).
narrative_ontology:measurement(magn_tr_t160, magna_carta_1215__universal_rights_reading, theater_ratio, 160, 0.28).
narrative_ontology:measurement_basis(magn_tr_t160, observed).
narrative_ontology:measurement(magn_tr_t240, magna_carta_1215__universal_rights_reading, theater_ratio, 240, 0.22).
narrative_ontology:measurement_basis(magn_tr_t240, observed).
narrative_ontology:measurement(magn_tr_t320, magna_carta_1215__universal_rights_reading, theater_ratio, 320, 0.18).
narrative_ontology:measurement_basis(magn_tr_t320, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(magn_be_t0, projected).
narrative_ontology:measurement(magn_be_t80, magna_carta_1215__universal_rights_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement_basis(magn_be_t80, observed).
narrative_ontology:measurement(magn_be_t160, magna_carta_1215__universal_rights_reading, base_extractiveness, 160, 0.22).
narrative_ontology:measurement_basis(magn_be_t160, observed).
narrative_ontology:measurement(magn_be_t240, magna_carta_1215__universal_rights_reading, base_extractiveness, 240, 0.26).
narrative_ontology:measurement_basis(magn_be_t240, observed).
narrative_ontology:measurement(magn_be_t320, magna_carta_1215__universal_rights_reading, base_extractiveness, 320, 0.28).
narrative_ontology:measurement_basis(magn_be_t320, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__universal_rights_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(magn_su_t0, projected).
narrative_ontology:measurement(magn_su_t80, magna_carta_1215__universal_rights_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement_basis(magn_su_t80, observed).
narrative_ontology:measurement(magn_su_t160, magna_carta_1215__universal_rights_reading, suppression_requirement, 160, 0.58).
narrative_ontology:measurement_basis(magn_su_t160, observed).
narrative_ontology:measurement(magn_su_t240, magna_carta_1215__universal_rights_reading, suppression_requirement, 240, 0.48).
narrative_ontology:measurement_basis(magn_su_t240, observed).
narrative_ontology:measurement(magn_su_t320, magna_carta_1215__universal_rights_reading, suppression_requirement, 320, 0.41).
narrative_ontology:measurement_basis(magn_su_t320, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% The constraint family (kernel: magna_carta_1215) decomposes into three empirically distinct constraints because each reading produces different ε values, different beneficiary/victim structures, and different type classifications. The universal_rights_reading (this story) claims negligible extraction and universal beneficiary scope; the baronial_privilege_reading claims negligible extraction for non-barons and limited beneficiary scope; the living_document_reading claims that the founding problem status is contestable because interpretive tradition, not the 1215 text, determines meaning. Each reading is authored independently as a clean ε-invariant constraint. The readings are linked via network.affects_constraints to show that they are sibling instantiations of the same kernel — the same evidentiary record about how Clause 39 has been interpreted and applied in history bears on all three, and the success of one reading constrains the feasibility of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
