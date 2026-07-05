% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections — Expansive (Functional Continuity) Reading
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the EXPANSIVE reading of the Lausanne minority
 *   protections kernel: that the 1923 treaty guarantees not merely individual
 *   worship but the functional continuity of pre-1923 religious governance —
 *   institutional self-administration, property rights, and clergy formation
 *   through theological schools — for Turkey's recognized non-Muslim
 *   minorities (principally the Ecumenical Patriarchate, the Armenian
 *   Patriarchate, and the Chief Rabbinate). Under this reading, the 1971
 *   closure of the Halki Seminary and ongoing property disputes are treaty
 *   violations, not domestic policy choices. This is a genuine coordination
 *   arrangement where it functions: it lets institutions plan
 *   multi-generational continuity instead of existing purely at
 *   administrative discretion. But its coordination benefit is conditional on
 *   interpretive victory — where the state does not concede the expansive
 *   reading, the same institutions bear the cost of asserting rights that go
 *   unenforced. The sibling readings (restrictive_reading: worship-only;
 *   guarantor_reading: internationally supervised) are NOT part of this
 *   constraint; each is a separate structurally distinct claim with its own ε
 *   and stakeholders, linked here only via cs_structure.reading_relations and
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ecumenical_patriarchate: primary institutional beneficiary and advocate of this reading
 *   - armenian_patriarchate_of_constantinople: parallel institutional beneficiary
 *   - chief_rabbinate_of_turkey: smallest, most vulnerable beneficiary institution
 *   - turkish_state: agenda-setter and de facto sole enforcer/discretionary interpreter
 *   - minority_religious_communities_dependent_on_state_recognition: bear the gap between claimed right and administered reality
 *   - greek_government: excluded from formal enforcement role under this domestic-interpretation reading
 *   - international_legal_scholars: analytical observers of the interpretive contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.55).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections — Expansive (Functional Continuity) Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '63ef5658-bf06-404b-beec-05aa8e0de546').
narrative_ontology:cs_kernel_codification('63ef5658-bf06-404b-beec-05aa8e0de546', fixed_text).
narrative_ontology:cs_authority_grounding('63ef5658-bf06-404b-beec-05aa8e0de546', practice).
narrative_ontology:cs_interpretation_layer_present('63ef5658-bf06-404b-beec-05aa8e0de546').
narrative_ontology:cs_reading_relation('63ef5658-bf06-404b-beec-05aa8e0de546', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('63ef5658-bf06-404b-beec-05aa8e0de546', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('63ef5658-bf06-404b-beec-05aa8e0de546', foundational, institutional_continuity_is_treaty_protected).
narrative_ontology:cs_axiom_status(institutional_continuity_is_treaty_protected, holdable).
narrative_ontology:cs_axiom_grounding('63ef5658-bf06-404b-beec-05aa8e0de546', institutional_continuity_is_treaty_protected, conventional).
narrative_ontology:cs_axiom('63ef5658-bf06-404b-beec-05aa8e0de546', secondary, domestic_courts_are_proper_adjudicators_of_institutional_claims).
narrative_ontology:cs_axiom_status(domestic_courts_are_proper_adjudicators_of_institutional_claims, holdable).
narrative_ontology:cs_axiom_grounding('63ef5658-bf06-404b-beec-05aa8e0de546', domestic_courts_are_proper_adjudicators_of_institutional_claims, conventional).
narrative_ontology:cs_reference_frame('63ef5658-bf06-404b-beec-05aa8e0de546', ottoman_millet_functional_succession).
narrative_ontology:cs_drift_state('63ef5658-bf06-404b-beec-05aa8e0de546', post_halki_closure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('63ef5658-bf06-404b-beec-05aa8e0de546', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, ecumenical_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_patriarchate_of_constantinople).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, chief_rabbinate_of_turkey).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_theological_seminaries).
narrative_ontology:constraint_victim(lausanne_minority_protections__expansive_reading, minority_religious_communities_dependent_on_state_recognition).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, treaty_functional_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims continuous institutional identity and self-administration dating to the pre-1923 order, including the right to train clergy and hold property as an institution rather than through ad hoc individual arrangements. Advocates for the expansive reading because its survival as an institution (not merely as a congregation of worshippers) depends on it. Cannot relocate; its seat, its properties, and its claim to primacy among Orthodox sees are geographically fixed in Istanbul.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, ecumenical_patriarchate, beneficiary,
    moderate, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__expansive_reading, ecumenical_patriarchate, agenda_setter).

% Administers Armenian community institutions, schools, and properties under the same functional-continuity logic. Depends on the expansive reading to maintain church-run schools and property holding structures against domestic legal pressure that would fold them into general foundation law.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_patriarchate_of_constantinople, beneficiary,
    moderate, civilizational, trapped, national).

% A small, demographically shrinking community whose institutional self-administration rights under the expansive reading provide the only legal basis for maintaining communal governance structures separate from generic civil society law. Has essentially no leverage to demand enforcement beyond appeals to the treaty text itself.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, chief_rabbinate_of_turkey, beneficiary,
    powerless, generational, trapped, national).

% Institutions like the Halki Seminary (closed since 1971) exist as claims rather than functioning bodies. The expansive reading is what makes their reopening or continued theoretical status defensible as a treaty right rather than a domestic policy discretion. They benefit from the reading in theory; in practice several have been shuttered by government action the state does not concede violates the treaty.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_theological_seminaries, beneficiary,
    powerless, generational, trapped, local).

% Administers the domestic legal environment in which the treaty is implemented, deciding case by case whether institutional self-administration, property claims, and seminary operation are honored, tolerated, restricted, or refused. Frames its own compliance narrowly, contesting the expansive reading's institutional claims (particularly clergy training and property title) while accepting individual worship guarantees. Holds essentially unilateral practical control over enforcement absent external pressure.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Ordinary congregants and lay administrators of minority foundations bear the practical cost when the expansive reading is asserted but not honored: unresolved property disputes, foundation boards subject to state-appointed trustees, absence of local clergy training forcing reliance on aging or foreign-trained clergy. They are the ones who experience the gap between the claimed right and its administration, without standing to litigate the treaty directly.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_religious_communities_dependent_on_state_recognition, payer,
    powerless, biographical, trapped, local).

% Has diplomatic and historical interest in Ecumenical Patriarchate continuity and periodically raises the issue bilaterally and in European fora, but has no formal enforcement role under the expansive (domestic-interpretation) reading, which locates adjudication inside Turkish domestic law rather than international supervision. Its advocacy is heard but not decisive.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, greek_government, excluded,
    institutional, generational, constrained, regional).

% Study the treaty text, its 1923 drafting history, and subsequent state practice to assess whether functional continuity was intended or read in. Provide competing legal opinions cited by all parties to the interpretive contest; hold no enforcement power themselves.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, treaty-anchored basis on which minority religious institutions can plan multi-generational continuity — training clergy, holding property, administering schools — rather than existing entirely at the discretion of shifting domestic policy, which is the coordination problem genuine institutional survival requires solving.
% TRANSFER_FUNCTION: When honored, the reading transfers institutional security (recognized property title, permission to train and ordain clergy, self-administration of communal governance) from the state's discretionary control to a treaty-guaranteed baseline for the named minority institutions. When contested or under-enforced, it transfers risk and administrative burden onto ordinary community members who must navigate ambiguous or hostile domestic implementation.
% ABSENT_VOICES: Individual congregants and junior clergy of the affected communities are rarely party to the interpretive contest itself, which is conducted between the Turkish state, patriarchal leadership, and international legal/diplomatic actors; their lived experience of unresolved property disputes or absent clergy training is cited in advocacy but they hold no direct standing in treaty interpretation.
% DISAPPEARANCE_RATIONALE: If the expansive reading were abandoned entirely (collapsing to the restrictive reading), the patriarchates and minority foundations argue their institutional existence — not merely worship — would become a matter of ordinary Turkish administrative and foundation law, exposing property and governance structures to reclassification. The Turkish state disputes that this would materially change anything, since it already treats most institutional questions as domestic matters in practice. Whether the world rearranges therefore depends on which reading was operative in practice versus in name — precisely the interpretive dispute this story documents.
% FOUNDING_PROBLEM: In 1923, departing from the population-exchange logic applied elsewhere, Lausanne carved out permanent non-Muslim minorities in Istanbul/Imbros/Tenedos who needed some guarantee that their communal religious and educational institutions — not just individual worship — would survive the transition from Ottoman millet governance to the Turkish nation-state.
% FOUNDING_PROBLEM_CORROBORATION: The patriarchates and diaspora legal advocates attest the founding problem remains live because institutional attrition (seminary closures, property disputes, demographic decline) continues. The Turkish state attests the founding problem was resolved by the transition itself and that remaining institutional questions are ordinary domestic administration, not treaty matters. Independent international legal scholarship (e.g., studies cited in Council of Europe and academic literature on Lausanne minority rights) corroborates that the institutional question remains genuinely unsettled in international law, rather than corroborating either interested party's preferred resolution.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, contested).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because, unlike a captured constraint, no party extracts rent from the arrangement functioning as claimed — the institutions asking for functional continuity are the ones who would use it, and the state's non-compliance produces institutional loss rather than institutional gain for the state. Suppression is authored moderate (0.55) reflecting the state's practical capacity to withhold recognition, close institutions (Halki, 1971), and impose administrative trustees on foundations without formal treaty violation being conceded — this is coercive but is asserted as domestic prerogative, not treaty enforcement, which is exactly the interpretive fight. Theater ratio (0.32) reflects genuine but partially symbolic institutional continuity: patriarchal seats persist and communities are recognized, but claimed rights (clergy training) sit dormant/unexercised for decades, producing an appearance of functioning arrangement that exceeds what actually operates. Accessibility collapse is moderate (0.45): communities cannot simply relocate their institutional claims elsewhere, but domestic Turkish courts and international diplomatic fora remain nominally available avenues, so alternatives have not fully collapsed. Resistance is fairly high (0.6): patriarchates, diaspora advocacy groups, and periodic international diplomatic pressure actively contest restrictive administrative practice, rather than acquiescing.
 *
 * DIRECTIONALITY LOGIC:
 *   The patriarchates and theological seminaries are declared beneficiaries because the expansive reading, where honored, subsidizes their institutional survival relative to the counterfactual of ordinary domestic law. They sit at powerless-to-moderate power with trapped exit (their institutional identity and physical seat cannot relocate), which the engine should read as amplifying effective extraction risk when the reading is NOT honored — the same low-power trapped position that makes them beneficiaries-in-theory makes them vulnerable-in-practice. The turkish_state is agenda_setter with institutional power and arbitrage-grade exit (it can selectively enforce, reinterpret, or simply decline without international consequence in most instances), placing it structurally outside the extraction relationship rather than as target or beneficiary in the conventional sense. Ordinary congregants are declared victims/payers: they bear the practical cost (unresolved property status, absent local clergy training pipelines) of unenforced institutional rights without holding standing to contest the interpretation themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting communal religious governance during the transition from Ottoman millet structure to the Turkish nation-state — is genuinely contested as live vs. dead: the patriarchates say the demographic and institutional attrition of their communities shows the protective function remains necessary; the state treats the transition as long complete and remaining institutional questions as ordinary administration. This story does NOT resolve that dispute (that is the guarantor_reading and restrictive_reading's business) — it documents that under the expansive interpretive frame, no single party has captured the arrangement for extraction; the risk is institutional atrophy (a Piton-adjacent path) rather than active predation, which the low extractiveness score and absence of a capturing beneficiary in gain_flow reflects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansive_vs_restrictive_textual_warrant,
    'Does the Lausanne treaty text and its 1923 drafting history support institutional self-administration, property rights, and clergy formation as treaty-guaranteed, or only individual freedom of worship?',
    'Comparative analysis of the treaty''s drafting history (League of Nations minority-protection instruments of the same era, negotiating record at Lausanne), alongside subsequent state practice and any authoritative international judicial or arbitral findings addressing the scope of Article 40-45 protections.',
    'If the textual/historical record clearly supports functional continuity, the restrictive reading''s foreclosure claim weakens substantially, strengthening this reading''s institutional protections; if the record supports only individual worship rights, this reading''s institutional claims (property, clergy formation) would be exposed as an interpretive extension rather than a textual guarantee.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_vs_restrictive_textual_warrant, conceptual, 'Whether the treaty text/history supports institutional as opposed to purely individual protections.').

omega_variable(
    domestic_vs_international_adjudication_locus,
    'Is the correct locus for adjudicating disputed institutional claims (e.g., Halki Seminary reopening, foundation property title) exclusively Turkish domestic courts and administration, or does it properly involve guarantor-state diplomacy and international human rights mechanisms?',
    'Track whether international bodies (ECtHR admissibility decisions, Council of Europe monitoring, guarantor-state diplomatic notes) are treated by Turkey and by the affected institutions as having binding or merely advisory relevance to these disputes over time.',
    'If international mechanisms gain practical traction, this expansive-but-domestic reading would functionally merge with or be superseded by the guarantor_reading, changing who the effective agenda_setter is; if international mechanisms remain advisory only, this reading''s domestic-adjudication premise holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_vs_international_adjudication_locus, empirical, 'Whether disputes are actually resolved domestically or through international pressure, over time.').

omega_variable(
    institutional_vs_actor_beneficiary_status,
    'Are the patriarchates and seminaries genuine beneficiaries of this reading, or is ''beneficiary'' status here largely theoretical given decades of non-exercise (e.g., Halki closed since 1971) — making them structurally closer to victims of an unenforced right than beneficiaries of an operative one?',
    'Track the ratio of years in which claimed institutional rights (seminary operation, unencumbered property title, self-administration of foundation boards) were actually exercised versus administratively blocked, across the interval.',
    'If non-exercise dominates, the beneficiary declaration should be read as aspirational/nominal rather than functioning, which would push the constraint''s effective classification toward piton (a claimed coordination function that has substantially atrophied) rather than rope, despite the low authored extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_actor_beneficiary_status, empirical, 'Whether the declared beneficiaries actually receive the claimed institutional benefits in practice or hold only a nominal, unexercised claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.15).
narrative_ontology:measurement_basis(laus_tr_t1923, observed).
narrative_ontology:measurement(laus_tr_t1960, lausanne_minority_protections__expansive_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement_basis(laus_tr_t1960, observed).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__expansive_reading, theater_ratio, 1971, 0.35).
narrative_ontology:measurement_basis(laus_tr_t1971, observed).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__expansive_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement_basis(laus_tr_t1990, observed).
narrative_ontology:measurement(laus_tr_t2010, lausanne_minority_protections__expansive_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement_basis(laus_tr_t2010, observed).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__expansive_reading, theater_ratio, 2024, 0.32).
narrative_ontology:measurement_basis(laus_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.12).
narrative_ontology:measurement_basis(laus_be_t1923, observed).
narrative_ontology:measurement(laus_be_t1960, lausanne_minority_protections__expansive_reading, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement_basis(laus_be_t1960, observed).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__expansive_reading, base_extractiveness, 1971, 0.24).
narrative_ontology:measurement_basis(laus_be_t1971, observed).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__expansive_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement_basis(laus_be_t1990, observed).
narrative_ontology:measurement(laus_be_t2010, lausanne_minority_protections__expansive_reading, base_extractiveness, 2010, 0.26).
narrative_ontology:measurement_basis(laus_be_t2010, observed).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__expansive_reading, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(laus_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.35).
narrative_ontology:measurement_basis(laus_su_t1923, observed).
narrative_ontology:measurement(laus_su_t1960, lausanne_minority_protections__expansive_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement_basis(laus_su_t1960, observed).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__expansive_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement_basis(laus_su_t1971, observed).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__expansive_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement_basis(laus_su_t1990, observed).
narrative_ontology:measurement(laus_su_t2010, lausanne_minority_protections__expansive_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement_basis(laus_su_t2010, observed).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__expansive_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(laus_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.1).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the lausanne_minority_protections kernel. expansive_reading (this story) claims institutional functional continuity is treaty-protected, adjudicated domestically. restrictive_reading claims only individual worship is treaty-protected, with institutional matters being ordinary domestic law — its core premise directly contradicts this reading's core premise (see forecloses edge in cs_structure). guarantor_reading shares this reading's premise that institutional continuity is protected but locates adjudication authority in international guarantor-state and human-rights mechanisms rather than Turkish domestic courts alone — this reading exerts downstream structural pressure on guarantor_reading's legitimacy conditions (see influences edge) because domestic non-enforcement under this reading is precisely the evidentiary basis guarantor_reading advocates cite for internationalizing adjudication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
