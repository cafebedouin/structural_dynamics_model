% ============================================================================
% CONSTRAINT STORY: trust_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trust_asymmetry, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trust_asymmetry
 *   human_readable: Trust Asymmetry in Institutional Relationships
 *   domain: social/institutional/relational
 *
 * SUMMARY:
 *   Trust asymmetry describes the structural condition where one party holds
 *   significantly more information about their own actions, reliability, or
 *   compliance than the other party can verify without bearing substantial
 *   costs. This constraint appears across institutional domains—healthcare
 *   (doctors vs patients), finance (corporations vs shareholders), employment
 *   (managers vs workers), governance (officials vs citizens)—and generates
 *   different classifications from different perspectives. The same
 *   structural phenomenon—the gap between what institutional actors know
 *   about themselves and what those who depend on them can independently
 *   verify—appears as an immutable law of information economics (mountain), a
 *   coordination mechanism that enables specialization (rope), a genuine
 *   hybrid mixing coordination benefits with asymmetric extraction (tangled
 *   rope), a temporary coordination failure being solved by transparency
 *   initiatives (scaffold), a degraded trust ritual (piton), or pure
 *   extraction (snare), depending on the observer's power, exit options, and
 *   time horizon. The constraint's extractiveness (0.58) reflects that
 *   institutional actors capture significant benefits from information
 *   asymmetry—reduced accountability costs, discretionary decision-making
 *   power, monopolistic pricing in the trust premium—while those dependent on
 *   trust bear the costs of verification barriers and breach exposure.
 *   Theater ratio (0.48) indicates moderate theatrical content: formal trust
 *   verification mechanisms (licensing, accreditation, audits) persist but
 *   with declining effectiveness relative to their performative functions.
 *
 * KEY AGENTS:
 *   - Trust-Bearing Individuals: Primary victims (powerless/trapped) — patients, employees, customers, citizens bearing trust without verification capacity; no organized exit or voice mechanisms
 *   - Organized Trust-Bearers: Secondary victims (moderate/constrained) — patient associations, unions, consumer groups; constrained by resource and regulatory barriers but with some agency
 *   - High-Status Institution: Primary beneficiary (institutional/arbitrage) — universities, hospitals, corporations, governments; captures reputation premium and reduces accountability costs; can arbitrage between jurisdictions
 *   - Captured Regulator: Secondary actor (institutional/constrained) — officially verifies trustworthiness but constrained by expertise gaps, funding dependence, revolving-door incentives; identity-locked to regulated sector
 *   - Transparency Movement: Organized reformers (organized/constrained) — FOIA advocates, open-data movements, auditing standards bodies; building alternative trust mechanisms with sunset logic
 *   - Institutional Trust Ritual: Systemic artifact (institutional/arbitrage) — formal certification and accreditation structures; maintains theater through institutional inertia despite declining verification function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent asymmetry as irreducible information economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trust_asymmetry, 0.58).
domain_priors:suppression_score(trust_asymmetry, 0.62).
domain_priors:theater_ratio(trust_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trust_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(trust_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(trust_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trust_asymmetry, tangled_rope).
narrative_ontology:human_readable(trust_asymmetry, "Trust Asymmetry in Institutional Relationships").
narrative_ontology:topic_domain(trust_asymmetry, "social/institutional/relational").

domain_priors:requires_active_enforcement(trust_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trust_asymmetry, high_status_institution).
narrative_ontology:constraint_beneficiary(trust_asymmetry, institutional_credentialed_actors).
narrative_ontology:constraint_victim(trust_asymmetry, asymmetric_information_bearers).
narrative_ontology:constraint_victim(trust_asymmetry, trust_bearing_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRUST-BEARING INDIVIDUAL (SNARE) — Cannot exit the asymmetry without abandoning institutions entirely. Bears full cost of institutional betrayal and information asymmetry. No organized voice; no mechanisms to verify claims before committing trust. Suppression is high: social stigma for 'trusting the wrong person,' reputational damage for reporting breaches, legal barriers to access information about institutional decision-making.
constraint_indexing:constraint_classification(trust_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED TRUST-BEARERS (TANGLED ROPE) — Patients' associations, employee unions, consumer groups. Constrained by resource limitations and regulatory barriers to full access. Benefit from institutional coordination (shared services, infrastructure, expertise) while bearing asymmetric information costs. Significant agency but not free mobility.
constraint_indexing:constraint_classification(trust_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-STATUS INSTITUTION (ROPE) — Experiences trust asymmetry as a coordination mechanism: reputation allows efficient operation without detailed public scrutiny. Can arbitrage between jurisdictions, switch disclosure standards, leverage brand equity. Net beneficiary of the constraint — extraction asymmetry flows toward this agent.
constraint_indexing:constraint_classification(trust_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPTURED REGULATOR (TANGLED ROPE) — Officially responsible for verifying institutional trustworthiness. Constrained by resource limitations, expertise capture, revolving-door incentives. Also benefits from the coordination function (trusts reduce transaction costs across the economy). Identity-locked to the regulated industry through career paths, funding dependence, and epistemic capture. Enforces the constraint actively through regulatory design while experiencing extraction as institutionally necessary.
constraint_indexing:constraint_classification(trust_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSPARENCY MOVEMENT (SCAFFOLD) — Organized agents (FOIA advocates, open-data movements, auditing standards bodies). Constrained by institutional resistance to disclosure but with a clear sunset logic: open information systems, algorithmic transparency standards, and third-party audit requirements are building alternative trust mechanisms. The constraint's extractiveness declines as these pathways mature. Suppression remains high during the transition, but exit is visible and approaching.
constraint_indexing:constraint_classification(trust_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL TRUST RITUAL (PITON) — The formal mechanisms of trust verification (credential certification, accreditation boards, professional licensing) have become largely performative. The rituals continue through institutional inertia—they provide theater that substitutes for genuine verification. Credentialing bodies no longer effectively police misconduct; accreditation has become commodified; licensing persists despite weak correlation to competence. Theater ratio is 0.48 overall, but the ritual perspective sees performance replacing function across governance structures.
constraint_indexing:constraint_classification(trust_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, information asymmetry is an irreducible feature of human knowledge: principals cannot fully verify agents' actions or expertise without bearing the cost themselves. This view treats trust asymmetry as a natural law—an immutable consequence of epistemic limits and the division of labor. However, the structural data contradicts the mountain classification: many asymmetries are institutional designs, not natural limits. The engine's false summit detection will reveal that 'asymmetry is natural' naturalizes what is actually a contingent governance choice.
constraint_indexing:constraint_classification(trust_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trust_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trust_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trust_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trust_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trust_asymmetry, TR),
    TR >= 0.70.

:- end_tests(trust_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Trust asymmetry generates measurable extraction for institutional actors: reduced compliance costs, pricing power from information advantage, discretionary decision-making, and reduced accountability exposure. The value reflects substantial but not total extraction—many institutions do maintain trustworthiness through career incentives, reputation concerns, and genuine expertise. The trajectory from 0.42 to 0.58 across the interval reflects historical accumulation: as regulatory capture has deepened and transparency norms have decayed, institutional actors have extracted more value from asymmetry. Suppression (0.62): High. Significant barriers prevent individual verification: expertise costs (understanding medical or financial complexity), access barriers (proprietary data, confidentiality restrictions), legal risks (NDAs, confidentiality agreements), and reputational costs (those who distrust institutions face social and material penalties). Theater ratio (0.48): Moderate. Formal trust verification mechanisms—accreditation boards, licensing exams, audit requirements—persist but with declining function. They generate theater (appearing to verify trustworthiness) while core asymmetries remain: audits verify compliance with procedures, not reliability of judgment; licensing verifies training completion, not ongoing competence; accreditation verifies institutional structure, not service quality. The modest theater ratio reflects that some verification function persists alongside the theatrical elements, unlike pure piton constraints.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here because the constraint benefits exactly the agents most able to exit (high-status institutions with arbitrage options) and extracts from exactly the agents least able to exit (trust-bearing individuals with trapped status). This creates radical disagreement about classification: snare from the trapped perspective, rope from the arbitrage perspective, tangled rope from the constrained perspective. The gap is not a measurement error—it is the structure of the constraint revealed through positional lenses.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation prioritizes beneficiary/victim declarations and exit options. Trust-bearing individuals are declared victims with trapped exit: high d. High-status institutions are declared beneficiaries with arbitrage exit: low d. Organized trust-bearers are secondary victims with constrained exit: moderate-high d. Captured regulators require an override: although nominally institutional/arbitrage, their identity lock to the regulated sector and constrained regulatory autonomy justify elevation of d from canonical 0.00 to ~0.35, reflecting that their apparent beneficiary status masks genuine extraction constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED BY PERSPECTIVAL STRUCTURE: The constraint is simultaneously extraction and coordination depending on where you stand. For the institution, trust asymmetry coordinates specialization and enables efficient operation—they experience rope. For the individual, the same mechanism extracts through information advantage—they experience snare. Neither classification is wrong; each is perspectival. The mandatrophy resolves not by choosing one type but by recognizing that the presheaf of perspectives over the observation site IS the constraint's true structure. The institutional perspective (rope) mistakes its partial view for the whole. The individual perspective (snare) mistakes its partial view for the whole. The analytical perspective risks mistake by universalizing the institutional framing (mountain: asymmetry is natural information economics) while ignoring the contingency of disclosure architecture. Tangled rope is the honest hybrid classification: genuine coordination function + genuine asymmetric extraction + active enforcement through regulatory design. The constraint's institutional reality is the spectrum of perspectives, not any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_reducibility,
    'Is information asymmetry an irreducible feature of complex institutions, or is it contingent on disclosure architecture and governance design?',
    'Comparative institutional analysis: organizations with high transparency (open-source projects, mutual aid networks, participatory governance models) vs. those with opacity. Measurement of trust outcomes and institutional failure rates across transparency levels.',
    'If irreducible: mountain classification approaches validity; trust asymmetry is inherent to scale and specialization. If contingent: asymmetry is an extractive design choice; snare and tangled rope perspectives dominate; scaffold and transparency pathways are structurally viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_reducibility, empirical, 'Whether information asymmetry is reducible through institutional design').

omega_variable(
    regulator_capture_mechanism,
    'Does regulatory capture in trust verification operate through economic incentives (revolving door, funding), epistemic capture (industry expertise sets standards), or identity fusion (regulator identity constituted through industry framing)?',
    'Analysis of regulator career paths, funding sources, and cognitive frameworks. Interviews or archival analysis showing whether captured regulators perceive their actions as enforcement of industry interests (economic) or as defense of legitimate institutional arrangements (epistemic/identity).',
    'If economic: override directionality for captured regulators to higher d values. If epistemic/identity: identity_locked exit options are structurally justified; perspectives require reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulator_capture_mechanism, empirical, 'Mechanism of regulatory capture in trust verification systems').

omega_variable(
    transparency_floor_effectiveness,
    'Do transparency interventions (FOIA, open data, audit requirements) actually reduce trust asymmetry, or do they create the theater of verification while preserving core asymmetries?',
    'Pre/post analysis of transparency interventions: measurement of information access, institutional responsiveness, and trust outcome changes. Determination of whether transparency reveals asymmetries or merely documents them without power redistribution.',
    'If effective: scaffold perspective is structurally sound; sunset clause is real; transparency pathways are genuine alternatives. If theater: transparency movements are pitons; asymmetry persists behind a new disclosure ritual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_floor_effectiveness, empirical, 'Whether transparency interventions reduce information asymmetry or create procedural theater').

omega_variable(
    trust_externality_scope,
    'Do institutional trust asymmetries impose externalities on non-participants (contaminated public health data, exploitative labor standards that set sectoral norms, corrupt financial standards that destabilize markets)?',
    'Network analysis of institutional coupling: identification of spillover effects from high-opacity institutions to peer institutions and broader market/governance systems. Measurement of information quality degradation across sectors when flagship institutions lose credibility.',
    'If high externalities: scope should expand from national to continental/global; chi values should increase via scope modifier. Constraint should be reclassified with larger scope. If low externalities: local/regional scopes are justified; extraction is more contained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_externality_scope, empirical, 'Scope of trust asymmetry externalities across institutional sectors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trust_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trust_tr_t0, trust_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(trust_tr_t10, trust_asymmetry, theater_ratio, 10, 0.42).
narrative_ontology:measurement(trust_tr_t20, trust_asymmetry, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(trust_be_t0, trust_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(trust_be_t10, trust_asymmetry, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(trust_be_t20, trust_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trust_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(trust_asymmetry, regulatory_capture).
narrative_ontology:affects_constraint(trust_asymmetry, information_commons_degradation).
narrative_ontology:affects_constraint(trust_asymmetry, fiduciary_duty_erosion).

% DUAL FORMULATION NOTE:
% Trust asymmetry is upstream of specific institutional failures (medical error, financial fraud, employment exploitation) but represents a distinct structural constraint. Each downstream constraint has its own specific extractiveness reflecting the domain-specific exploitation mechanism; trust asymmetry has its own extractiveness reflecting the general structural advantage information inequality confers. The network links show how degradation in trust asymmetry feeds into regulatory capture and information commons degradation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trust_asymmetry, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
