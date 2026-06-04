% ============================================================================
% CONSTRAINT STORY: informational_self_determination__census_origin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informational_self_determination__census_origin_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: informational_self_determination__census_origin_reading
 *   human_readable: Informational Self-Determination: The 1983 Census Origin Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   In 1983, the German Constitutional Court (Bundesverfassungsgericht)
 *   issued a judgment refusing to compel census participation on the grounds
 *   that individuals possess a constitutional right to control information
 *   about themselves — a right derived from human dignity and personality
 *   (Persönlichkeitsrecht). This was not a judgment about privacy in the
 *   modern sense; it was a judgment about the state's power to enumerate
 *   citizens without consent. The court held that mandatory, untargeted
 *   census enumeration violated dignity because it reduced the person to a
 *   data point in the state's total knowledge system. This reading of
 *   informational self-determination is distinctive: it is not about data
 *   protection as a regulatory framework, and it is not about limiting
 *   surveillance after the fact. It is about the suppression of unconsented
 *   enumeration itself — the refusal to become a counted unit in the state's
 *   machinery. The constraint exhibits high suppression (census refusal is
 *   illegal; the state enforces enumeration through penalties) and moderate
 *   extractiveness (the state extracts information; citizens gain the right
 *   to condition use on dignitary grounds, but cannot prevent enumeration).
 *   The measurement trajectory shows declining extractiveness over the
 *   30-year interval as purpose-limitation rules and data protection law
 *   mature, reducing the state's ability to use enumerated data for
 *   unconsented secondary purposes.
 *
 * KEY AGENTS:
 *   - Counted Citizens (Census Refusers): Primary beneficiary (powerless/trapped) — gain a right to refuse or condition enumeration on dignity grounds, but face legal penalties for refusal
 *   - Total-Information State Designs: Primary victim (institutional/constrained) — the constraint suppresses their capacity for untargeted enumeration and unrestricted secondary data use
 *   - Bundesverfassungsgericht (Constitutional Court): Secondary actor (institutional/arbitrage) — establishes the right, creates ongoing jurisdiction to review enumeration and data-handling regimes
 *   - State Enumeration Authority (Census Bureau, Police Registries): Institutional actor (institutional/constrained) — must conduct necessary census and maintain registries but faces heightened scrutiny for unconsented uses
 *   - Later Regulatory Architecture (Data Protection Authorities): Institutional beneficiary (institutional/arbitrage) — gains constitutional anchor for purpose-limitation and minimization principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informational_self_determination__census_origin_reading, 0.48).
domain_priors:suppression_score(informational_self_determination__census_origin_reading, 0.65).
domain_priors:theater_ratio(informational_self_determination__census_origin_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informational_self_determination__census_origin_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(informational_self_determination__census_origin_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(informational_self_determination__census_origin_reading, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informational_self_determination__census_origin_reading, tangled_rope).
narrative_ontology:human_readable(informational_self_determination__census_origin_reading, "Informational Self-Determination: The 1983 Census Origin Reading").
narrative_ontology:topic_domain(informational_self_determination__census_origin_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(informational_self_determination__census_origin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(informational_self_determination__census_origin_reading, '95444383-2eb8-455d-a09a-b0f0f68bdd01').
narrative_ontology:cs_kernel_codification('95444383-2eb8-455d-a09a-b0f0f68bdd01', formalized).
narrative_ontology:cs_authority_grounding('95444383-2eb8-455d-a09a-b0f0f68bdd01', lineage).
narrative_ontology:cs_interpretation_layer_present('95444383-2eb8-455d-a09a-b0f0f68bdd01').
narrative_ontology:cs_reading_relation('95444383-2eb8-455d-a09a-b0f0f68bdd01', informational_self_determination__data_protection_constitutionalized_reading, influences).
narrative_ontology:cs_reading_relation('95444383-2eb8-455d-a09a-b0f0f68bdd01', informational_self_determination__surveillance_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('95444383-2eb8-455d-a09a-b0f0f68bdd01', foundational, enumeration_requires_consent).
narrative_ontology:cs_axiom_status(enumeration_requires_consent, holdable).
narrative_ontology:cs_axiom_grounding('95444383-2eb8-455d-a09a-b0f0f68bdd01', enumeration_requires_consent, deontological).
narrative_ontology:cs_axiom('95444383-2eb8-455d-a09a-b0f0f68bdd01', foundational, dignity_grounds_data_control).
narrative_ontology:cs_axiom_status(dignity_grounds_data_control, holdable).
narrative_ontology:cs_axiom_grounding('95444383-2eb8-455d-a09a-b0f0f68bdd01', dignity_grounds_data_control, deontological).
narrative_ontology:cs_reference_frame('95444383-2eb8-455d-a09a-b0f0f68bdd01', dignity_based_informational_autonomy).
narrative_ontology:cs_drift_state('95444383-2eb8-455d-a09a-b0f0f68bdd01', contemporary_digital_enumeration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('95444383-2eb8-455d-a09a-b0f0f68bdd01', '').
narrative_ontology:cs_kernel_id(informational_self_determination__census_origin_reading, informational_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informational_self_determination__census_origin_reading, counted_citizens).
narrative_ontology:constraint_victim(informational_self_determination__census_origin_reading, total_information_state_designs).
narrative_ontology:constraint_victim(informational_self_determination__census_origin_reading, mass_enumeration_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENUMERATED INDIVIDUAL (SNARE) — The counted person has no exit from enumeration once the state's machinery has registered them. Refusal is punished; inclusion is mandatory. The extraction is total: one's name, address, demographic identity become state property without consent. Suppression is complete — legal penalties for refusal, no alternative enumeration regime, no real exit option. The 1983 reading holds that this snare must be severed: control over one's data is a right to refuse enumeration or condition its use on dignitary grounds.
constraint_indexing:constraint_classification(informational_self_determination__census_origin_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL COURT (TANGLED ROPE) — The court benefits from establishing a novel right (expands constitutional jurisdiction, creates ongoing litigation docket, positions court as rights arbiter). But it also faces genuine coordination problem: how does a state conduct necessary census while respecting dignity? The 1983 judgment creates both a barrier (suppression of unconsented enumeration) and a coordinating principle (purpose and consent condition data use). The extraction is moderate: the court gains institutional power to review data-handling regimes; the counted citizen gains a right but constrained by necessity.
constraint_indexing:constraint_classification(informational_self_determination__census_origin_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY ARCHITECTURE (ROPE) — The 1983 reading seeded the later codification: purpose limitation, data minimization, consent-based handling. From the perspective of the regulatory apparatus, this is pure coordination — the constraint establishes the legitimate frame within which data handling becomes lawful. No extraction: the regulatory regime benefits by having a constitutional anchor for its rules. Citizens benefit by having enforceable minimization principles. Both win from clarity.
constraint_indexing:constraint_classification(informational_self_determination__census_origin_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ENUMERATION AUTHORITY (TANGLED ROPE) — The state must conduct census and maintain registries for legitimate governance (coordination function). But the 1983 reading restricts how it can use enumerated data: consent for secondary uses, purpose limitation, suppression of untargeted surveillance. The extraction flow is bidirectional: the state wants unrestricted enumeration (extraction from citizens); the constraint forces purpose-limitation (extraction from state's total-information capacity). Suppression is high because the state has legal tools to punish refusal, but the constraint narrows their legitimacy.
constraint_indexing:constraint_classification(informational_self_determination__census_origin_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective that treats human dignity as an irreducible foundation, informational self-determination appears as a natural law: dignity cannot be overridden by state inventory. The right is unchangeable because dignity is unchangeable. This perspective holds that enumeration without consent is inherently illicit — not because of policy calculation but because dignity permits no alternative. However, the structural data contradicts pure mountain classification: the constraint requires active enforcement (court review), victims are identifiable (citizens), and beneficiaries exist. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(informational_self_determination__census_origin_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informational_self_determination__census_origin_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informational_self_determination__census_origin_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informational_self_determination__census_origin_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(informational_self_determination__census_origin_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(informational_self_determination__census_origin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The state extracts enumerated data; citizens gain a right to condition secondary uses on consent. The extraction is not total (snare-level) because the right provides some agency; it is not zero (rope-level) because mandatory enumeration persists. The measurement trajectory shows decline from 0.72 (1983, before purpose-limitation law matured) to 0.48 (2013, after purpose-limitation and consent requirements became enforceable). Suppression (0.65): Moderate-high. Suppression of refusal is structural: legal penalties, no alternative enumeration regime, no real exit from being counted. However, suppression is not absolute (0.80+) because the constraint permits conditioned use and the court provides a review mechanism. Theater ratio (0.30): Low. Unlike traditional data protection discourse, the 1983 reading is not performative — the judgment directly addresses enumeration as a state action, not as a regulatory/administrative category. The constraint is about denying the state a power (unconsented enumeration), not about creating a compliance regime. The low theater reflects that the constraint operates at the level of constitutional structure, not institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   The census_origin_reading creates a diagnostic gap between naturalizing and constructivist interpretations. From the dignitarian (natural law) perspective, the right to refuse enumeration is immutable because dignity is immutable — this observer sees a mountain. From the analytical perspective that tracks contingency, the right emerged from a specific 1983 judgment responding to a specific state capacity (the computerized census) and represents a constructed constraint on state power, not a natural law. The gap reveals whether dignity functions as an irreducible ground or as a framing device for resisting state enumeration. The beneficiary (counted citizen) and victim (total-information state) perspectives differ radically: the citizen sees a right (snare transformed into constrained agency); the state sees a barrier (rope transformed into tangled_rope by the constraint). The court perspective sees pure coordination (establishing legitimate enumeration bounds). The measurement trajectory (declining extractiveness) shows that the constraint's force diminishes as purpose-limitation law matures — secondary suppression erodes the primary suppression of unconsented enumeration as enforcement improves.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from their structural position: Counted citizens (powerless/trapped) experience maximum extraction (high d, ~0.95) because they face mandatory enumeration with no exit and no compensation. The constitutional court (institutional/arbitrage) experiences low extraction (d ~0.20) because they benefit from establishing the right and have exit options (decline jurisdiction, narrow the right's scope). The state enumeration authority (institutional/constrained) experiences moderate extraction (d ~0.55) because they want unrestricted enumeration but face the constraint. The effective extractiveness (chi) for each perspective is scaled by the sigmoid f(d) and by scope σ(S). The national scope (σ=1.0) does not amplify or suppress the extraction. The perspectival gap emerges from different d values: the counted citizen sees snare (f(d)≈1.42, high chi); the court sees rope (f(d)≈-0.12, negative chi); the state authority sees tangled_rope (f(d)≈0.75, moderate chi). The analytical observer at universal scope risks seeing mountain (treating dignity as unchangeable natural law), but the structural data reveals this as false summit: the constraint requires active enforcement, beneficiaries and victims are identifiable, and the right is historically contingent on the 1983 judgment.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1983 reading resolves mandatrophy by locating the constraint's function in suppression rather than in pure extraction. The constraint is not about controlling data once collected (that is the data_protection_constitutionalized_reading). It is about refusing the enumeration act itself. This shifts the type from snare (which would emphasize total extraction with no beneficiary function) to tangled_rope: genuine coordination function (establishing legitimate enumeration bounds) combined with asymmetric extraction (the state wants unrestricted enumeration, citizens gain conditionality). The piton hypothesis (is this just performative theater?) is rejected by the low theater ratio (0.30) and the active enforcement requirement. The constraint is not a degraded ritual; it is a living doctrine that Karlsruhe applies to each new enumeration or data-retention scheme. The mountain hypothesis (is this a natural law of dignity?) is tested by the false summit mechanism: if beneficiaries exist and are identifiable (they are — counted citizens), the engine flags the mountain as potentially a false summit, requiring the analytical observer to justify the natural law claim. The 1983 reading cannot make that justification hold — dignity alone does not explain why the constraint emerged specifically in response to computerized enumeration, nor why the constraint's scope narrows as purpose-limitation law matures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_foundations_ambiguity,
    'Is informational self-determination a natural right grounded in inherent human dignity, or a constructed right that emerged from contingent mid-20th-century concerns about state power and databases?',
    'Genealogical analysis of dignity concept across legal traditions; comparison of pre-database enumeration practices vs. post-database data protection framing; determination of whether the ''dignity'' claim tracks philosophical consensus or represents a particular German legal innovation',
    'If natural/universal: the right is unchangeable, boundaries are intuitive, applies to all enumeration contexts. If constructed: the right is revisable, boundaries are contingent on data landscape, may not apply to non-enumeration contexts (genetic data, biometric scanning, voluntary digital traces).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_foundations_ambiguity, conceptual, 'Whether dignity-grounded informational self-determination is natural law or constructed right').

omega_variable(
    purpose_limitation_enforceability,
    'Can purpose limitation actually be enforced when data collection and data use are separated in time and institutional locale? Does the constraint prevent actual secondary misuse or merely create a liability framework?',
    'Empirical study of enforcement outcomes in German/EU data protection cases; tracking of secondary-use violations and remedies; assessment of whether penalty structures create sufficient deterrence for institutional violators vs. individual beneficiaries',
    'If enforceable: constraint is tangled_rope (genuine coordination function + extraction limits). If unenforced: constraint devolves toward piton (performative declaration) or snare (constraint on citizens only, not state).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(purpose_limitation_enforceability, empirical, 'Whether purpose limitation can be effectively enforced across institutional time').

omega_variable(
    kernel_reading_contest_structure,
    'Which sibling reading of the informational_self_determination kernel is this census_origin_reading actually supporting or foreclosing?',
    'Historical analysis of how the 1983 judgment was cited in later German and EU data protection cases; whether subsequent jurisprudence treated the census refusal principle as foundational (supporting all data uses) or narrower (applying only to enumeration). Identification of whether the reading logically entails or coexists with the data_protection_constitutionalized and surveillance_proportionality readings.',
    'If the 1983 reading forecloses alternatives: the kernel has logical structure that rules out some readings when the census principle is adopted. If coexists: the three readings represent genuinely different but compatible interpretive traditions. If influences: the census reading creates institutional/doctrinal pressure on the others without ruling them out.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'The logical and doctrinal relationships between the census reading and its sibling readings of the informational_self_determination kernel').

omega_variable(
    consent_pragmatics_under_state_power,
    'When the 1983 reading grounds the right in voluntary consent and dignitary refusal, does this assume that refusal is actually possible without severe penalty? If the state can punish census refusal (fines, legal liability), is consent ''voluntary''?',
    'Legal analysis of whether German law permits actual refusal of census enumeration without penalty; comparison with jurisdictions that tolerate refusal; assessment of whether the dignity framing requires penalty-free exit or merely legal justification for penalty.',
    'If exit is penalized: the right is formally present but practically a mountain (unchangeable constraint on citizens without genuine agency). If exit is free: the right is genuinely tangled_rope (coordination with real choice). If courts have created penalty-free zones: the right''s scope is narrower than its framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_pragmatics_under_state_power, empirical, 'Whether consent to enumeration is actually voluntary given penalty structures for refusal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informational_self_determination__census_origin_reading, 1983, 2013).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(info_be_t0, informational_self_determination__census_origin_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(info_be_t5, informational_self_determination__census_origin_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(info_be_t10, informational_self_determination__census_origin_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(info_su_t0, informational_self_determination__census_origin_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(info_su_t5, informational_self_determination__census_origin_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(info_su_t10, informational_self_determination__census_origin_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informational_self_determination__census_origin_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(informational_self_determination__census_origin_reading, informational_self_determination__data_protection_constitutionalized_reading).
narrative_ontology:affects_constraint(informational_self_determination__census_origin_reading, informational_self_determination__surveillance_proportionality_reading).

% DUAL FORMULATION NOTE:
% The informational_self_determination kernel decomposes into three structurally distinct constraint stories with different ε values and enforcement mechanisms. The census_origin_reading (this story) focuses on suppression of unconsented enumeration (high suppression, moderate extractiveness). The data_protection_constitutionalized_reading focuses on purpose-limitation and regulatory architecture (lower suppression, lower extractiveness, higher theater as administrative compliance emerges). The surveillance_proportionality_reading focuses on ongoing review of retention and screening (medium suppression, medium extractiveness, ongoing institutional engagement). Each reading is a separate constraint story linked via network.affects_constraints. They represent three interpretive traditions descended from the same kernel: enumeration suppression → regulatory architecture → surveillance proportionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(informational_self_determination__census_origin_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
