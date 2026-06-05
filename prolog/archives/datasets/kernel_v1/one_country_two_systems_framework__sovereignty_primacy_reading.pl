% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_sovereignty_primacy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems: Sovereignty Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   The 'One Country, Two Systems' (OCTS) framework for Hong Kong, formalized
 *   in the Sino-British Joint Declaration (1984) and codified in Hong Kong's
 *   Basic Law (1990), is a contested kernel grounding its legitimacy in a
 *   fixed text whose meaning different parties interpret radically
 *   differently. This constraint instantiates the SOVEREIGNTY PRIMACY
 *   READING: the interpretation that holds Hong Kong's autonomy is delegated
 *   by and revocable through PRC sovereign authority; that national security
 *   and territorial integrity override local autonomy when they conflict.
 *   Under this reading, the 2020 National Security Law represents not a
 *   violation of OCTS but a clarification of its true meaning — PRC authority
 *   is supreme, and HK governance exists only insofar as it serves PRC
 *   interests. The constraint exhibits snare characteristics: high
 *   extractiveness (0.68), high suppression (0.72), and moderate theater
 *   (0.58). The theater ratio is lower than pure performative constraints
 *   because enforcement is substantive (arrests, prosecutions, judiciary
 *   subordination) rather than purely ceremonial. Extractiveness rises
 *   sharply from 2019 (0.35 pre-NSL) to 2025 (0.68 post-NSL), tracking the
 *   implementation of security law and the erosion of civil liberties.
 *   Suppression follows the same trajectory, reflecting the
 *   institutionalization of legal restrictions on speech, assembly, and
 *   association. The extraction mechanism is state coercion embedded in law:
 *   autonomy is preserved in form (HK retains nominal governmental
 *   structures) but extracted in substance (those structures cannot act
 *   against mainland interests). This reading directly FORECLOSES the
 *   autonomy primacy reading: if sovereignty is supreme and revocable, then
 *   autonomy cannot be substantive or treaty-guaranteed. It INFLUENCES the
 *   balanced coexistence reading by asserting that political accommodation
 *   occurs only within a framework of acknowledged PRC supremacy.
 *
 * KEY AGENTS:
 *   - Central PRC Authority: Beneficiary (institutional/arbitrage) — extracts political control, regime security, and territorial supremacy under the guise of maintaining OCTS
 *   - Mainland Security Apparatus: Beneficiary (institutional/arbitrage) — operates in HK with legal immunity under NSL; suppresses opposition through coordinated enforcement
 *   - Hong Kong Civil Society and Political Opposition: Victims (powerless/trapped) — cannot exit HK territory; face criminal liability for speech and assembly under NSL
 *   - Hong Kong Judiciary: Victim (powerful/constrained) — structurally independent but subordinated on national security matters; legitimacy extracted to serve PRC authority
 *   - International Community (UK, US, UN, treaty witnesses): Organized actor (organized/constrained) — acknowledge OCTS but lack enforcement mechanism; benefit from HK stability but constrained from intervening in 'internal' affairs
 *   - Analytical Observer: (analytical/analytical) — risks naturalizing sovereignty as immutable law, obscuring the contested kernel beneath the surface.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.68).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.72).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, snare).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764').
narrative_ontology:cs_kernel_codification('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', fixed_text).
narrative_ontology:cs_authority_grounding('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', extraction).
narrative_ontology:cs_interpretation_layer_present('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764').
narrative_ontology:cs_reading_relation('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', foundational, prc_sovereignty_supreme_over_delegated_autonomy).
narrative_ontology:cs_axiom_status(prc_sovereignty_supreme_over_delegated_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', prc_sovereignty_supreme_over_delegated_autonomy, deontological).
narrative_ontology:cs_axiom('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', foundational, national_security_overrides_civil_governance).
narrative_ontology:cs_axiom_status(national_security_overrides_civil_governance, holdable).
narrative_ontology:cs_axiom_grounding('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', national_security_overrides_civil_governance, deontological).
narrative_ontology:cs_reference_frame('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', prc_supreme_authority_framework).
narrative_ontology:cs_drift_state('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', post_2020_national_security_law_era, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('2bfa8afa-4dd2-4bd4-8dcd-5cafa181c764', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, central_prc_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_apparatus).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, political_opposition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HK CIVIL SOCIETY (SNARE) — Trapped within territorial boundaries; cannot exit the legal jurisdiction without abandoning livelihood, family, property. National Security Law creates asymmetric speech/assembly restrictions with no court remedy (judiciary subordinated to security considerations). Maximum experienced extraction: state coercion apparatus operates with minimal constraint; alternatives suppressed through law and enforcement threat.
constraint_indexing:constraint_classification(one_country_two_systems_framework__sovereignty_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HK JUDICIARY (SNARE) — Structurally powerful (independent courts under OCTS), but constrained by security law jurisdiction: cannot rule against mainland interests on national security; loses interpretive independence on constitutional questions affecting sovereignty. Extraction mechanism: judiciary's legitimacy is extracted to serve PRC authority while maintaining surface autonomy. High suppression: institutional pressure to self-censor limits judicial adventurousness.
constraint_indexing:constraint_classification(one_country_two_systems_framework__sovereignty_primacy_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRC CENTRAL AUTHORITY (ROPE) — Experiences the constraint as coordination: maintaining HK within the state while preserving economic utility requires managing stability. National Security Law coordinates PRC authority with HK legal system, allowing mainland enforcement while preserving market confidence. Beneficiary perspective: extraction flows toward mainland authority; autonomy becomes instrument for resource extraction and regime security.
constraint_indexing:constraint_classification(one_country_two_systems_framework__sovereignty_primacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL COMMUNITY (TANGLED ROPE) — Organized (UN, UK, US, international legal bodies) but constrained by lack of enforcement mechanism. OCTS operates simultaneously as coordination device (bilateral trade, financial hub status) and extraction mechanism (PRC authority overriding treaty commitments). International actors have genuine interest in OCTS functioning but constrained leverage; coordination benefit (HK stability, financial integration) coupled with asymmetric extraction (treaty meaning unilaterally rewritten by Beijing).
constraint_indexing:constraint_classification(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / SOVEREIGNTY NATURALIZATION (MOUNTAIN) — From civilizational/universal scope, state sovereignty is treated as axiomatic: territorial entities always have supreme authority over internal affairs; subordinate units (provinces, special regions) necessarily have revocable autonomy; national security always overrides local governance. This perspective sees the sovereignty primacy reading as reflecting immutable principles of international law and state structure. Engine detects this as potential false summit: the naturalization of sovereignty as law obscures that OCTS is a treaty commitment with a specific written text (Joint Declaration, Basic Law) whose meaning is contested.
constraint_indexing:constraint_classification(one_country_two_systems_framework__sovereignty_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(one_country_two_systems_framework__sovereignty_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(one_country_two_systems_framework__sovereignty_primacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The sovereignty primacy reading operationalizes a mechanism where HK's formal autonomy becomes an instrument for extracting legitimacy (HK appears self-governing), resources (financial hub serves PRC economy), and political control (opposition suppressed through ostensibly local law). The extraction is not maximal (0.85+) because some genuine administrative autonomy persists in non-political domains (finance, trade); but in political speech, assembly, and governance decisions affecting regime security, extraction is severe. The upward trajectory (0.35→0.68) reflects the post-2019 institutionalization of security law — extraction was latent in the OCTS text but became manifest in enforcement. Suppression (0.72): High. National Security Law creates a legal framework where alternatives to PRC-preferred governance are criminalized. Speech can be prosecuted as subversion; assembly can be criminalized as secession; opposition candidates are barred from running. Suppression is not total (0.85+) because some dissent persists in limited forums and some citizens can emigrate, but the institutional suppression of alternatives is substantial. Theater ratio (0.58): Moderate. The sovereignty primacy constraint combines substantive enforcement (actual arrests, trials, convictions) with performative elements (maintaining appearances of autonomy, rule of law, judicial independence in non-security domains). The theater is lower than piton-level constraints because the coercion is real and consequential, not merely ceremonial. The declining trajectory (0.68→0.58) reflects that the theater diminishes as enforcement becomes normalized — post-2020, the National Security Law is openly acknowledged as subordinating HK governance to PRC interests, reducing the need for performative autonomy claims.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival gap. The PRC central authority sees coordination (Rope perspective) — the constraint maintains HK stability while securing PRC supremacy. The HK civil society sees pure extraction (Snare) — their alternatives are criminalized and they cannot exit. The judiciary sees constrained power (Snare from a powerful institutional position) — formally independent but substantively subordinated. The international community sees a mixed coordination-extraction dynamic (Tangled Rope) — genuine economic coordination benefits coupled with asymmetric political extraction. The analytical observer risks seeing immutable sovereignty law (Mountain) — treating PRC supremacy as a natural feature of state structure rather than a contestable interpretation of a specific treaty. The engine's false summit detector should flag this mountain classification as naturalization of a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for this constraint differs by perspective based on agent structural position and exit capacity. Central PRC authority (beneficiary, institutional power, arbitrage exit) derives d ≈ 0.05 (full beneficiary position) — the constraint flows toward them; f(d) ≈ -0.12, yielding negative effective extraction (they profit from the constraint, experiencing it as coordination). Hong Kong civil society (victim, powerless, trapped) derives d ≈ 0.95 (full target) — the constraint flows away from them; f(d) ≈ 1.42, yielding maximum experienced extraction. The judiciary (victim, powerful, constrained) derives d ≈ 0.75 — structurally capable but institutionally constrained by security law; f(d) ≈ 1.10. International actors (organized, constrained) derive d ≈ 0.55; f(d) ≈ 0.75. The scope modifier σ(regional) = 0.9 applies (regional scope slightly dampens chi compared to global scope). The analytical observer (analytical power, analytical exit) derives d ≈ 0.72 via canonical fallback; f(d) ≈ 1.15. No directionality overrides are needed — the derivation chain produces empirically accurate directionalities from the declared beneficiary/victim structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT (ε = 0.68 > 0.70 gate approaches): The sovereignty primacy reading must resolve the tension between coordination (OCTS was designed as a coordination mechanism enabling economic integration and stable governance) and extraction (the security law operationalizes extraction of political control and regime legitimacy). The mandatrophy is resolved by recognizing that this reading asserts coordination IS subordinated to sovereignty — the coordination function (HK economic utility, financial hub status) exists only insofar as it serves PRC extraction (regime security, territorial control). Under the sovereignty primacy reading, there is no genuine mandatrophy because the ranking is clear: sovereignty (extraction) is supreme; autonomy-coordination is instrumental. The mandatrophy appears only from alternative readings (autonomy primacy, balanced coexistence) that assert coordination and sovereignty have independent or equal standing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    joint_declaration_legal_status,
    'Does the Sino-British Joint Declaration (1984) constitute an international treaty with binding force on unilateral interpretation, or is it a non-binding memorandum that PRC can unilaterally rewrite?',
    'International Court of Justice review (if invoked); UN treaty authority determinations; diplomatic recognition of declaration''s legal status by major powers; analysis of Hong Kong''s legal standing to challenge PRC actions in international forums',
    'If treaty: autonomy is constrained but has external enforcement mechanism; sovereignty primacy reading becomes a treaty violation (ε shifts downward, classification toward tangled_rope). If non-binding: PRC authority has no external limit; sovereignty primacy reading reflects actual structural reality; ε remains high, snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(joint_declaration_legal_status, conceptual, 'Legal status and enforceability of the Sino-British Joint Declaration').

omega_variable(
    national_security_law_scope_boundary,
    'Where does the National Security Law''s jurisdiction end and HK civil law begin? Are speech, assembly, and association subject to national security review, or are they protected zones?',
    'Hong Kong Court of Final Appeal rulings on NSL scope (currently limited by subordination of judiciary); international human rights body assessments (UN Human Rights Committee, Amnesty International); comparative analysis with other national security regimes; tracking of prosecutions under NSL to identify de facto boundaries',
    'If civil liberties are protected zones: suppression is lower than 0.72; many acts of civil society are genuinely outside state coercion scope; classification moves toward tangled_rope. If entire civil sphere is potentially subject to NSL review: suppression is confirmed at 0.72+; snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_security_law_scope_boundary, empirical, 'Scope and boundaries of National Security Law jurisdiction in Hong Kong').

omega_variable(
    treaty_kernel_unilateral_revision,
    'Can a party to a treaty unilaterally rewrite the treaty''s meaning while maintaining its legal form and the other party''s nominal agreement?',
    'Vienna Convention on the Law of Treaties interpretation; historical examples of treaty-form-preservation with unilateral meaning drift (e.g., post-Soviet state relationships, post-colonial constitutional frameworks); distinguishing between treaty amendment (requiring renegotiation) and treaty interpretation (unilateral authority?)',
    'If unilateral revision is legally permissible: the sovereignty primacy reading represents a legitimate (if controversial) use of state authority; kernel contest is political, not legal; ε reflects the political extraction, not a treaty violation. If unilateral revision violates treaty law: PRC actions constitute treaty breach; the sovereignty primacy reading becomes a defense of illegal action; ε interpretation shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_kernel_unilateral_revision, conceptual, 'Whether treaty meaning can be unilaterally revised while preserving treaty form').

omega_variable(
    autonomy_revocability_axiom,
    'Is autonomy categorically revocable at will by a sovereign superior, or does autonomy imply some irreducible guarantee even if formally delegated?',
    'Constitutional theory analysis of autonomy vs delegation; comparative study of federal systems (US, Canada, EU) and their autonomy-revocation provisions; philosophical distinctions between conditional and unconditional autonomy; analysis of other OCTS formulations in history (Hong Kong 1841-1997, Macau, Taiwan independence claims)',
    'If autonomy is conditionally revocable: sovereignty primacy reading is theoretically coherent; the constraint''s snare classification is justified. If autonomy implies irreducible guarantees: sovereignty primacy reading is self-contradictory (autonomy + perfect revocability = nullity); classification shifts or omegas multiply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_revocability_axiom, conceptual, 'Whether delegated autonomy logically implies revocability or irreducible guarantees').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 2019, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octsp_theater_2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(octsp_theater_2022, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 3, 0.62).
narrative_ontology:measurement(octsp_theater_2025, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(octsp_extractiveness_2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(octsp_extractiveness_2022, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(octsp_extractiveness_2025, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(octsp_suppression_2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(octsp_suppression_2022, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(octsp_suppression_2025, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judicial_independence).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_liberties_restrictions).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_law_jurisdiction_scope).

% DUAL FORMULATION NOTE:
% The one_country_two_systems_framework is a contested kernel with three distinct readings (autonomy_primacy, balanced_coexistence, sovereignty_primacy), each instantiating a different constraint with different ε values and classifications. This file generates the sovereignty_primacy reading only. All three readings share the same kernel text (Joint Declaration, Basic Law) but interpret its meaning radically differently. The three constraints are linked via network.affects_constraints to show that changes in one reading (e.g., international legal challenge to PRC interpretation) would impact the others. The sovereignty primacy reading is downstream of institutional commitment to PRC supremacy and upstream of concrete constraints on civil liberties and judicial independence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
