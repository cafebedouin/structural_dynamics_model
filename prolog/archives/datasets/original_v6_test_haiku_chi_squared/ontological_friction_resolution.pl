% ============================================================================
% CONSTRAINT STORY: ontological_friction_resolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ontological_friction_resolution, []).

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
 *   constraint_id: ontological_friction_resolution
 *   human_readable: The Chaste Fire of Truth — Ontological Friction Resolution
 *   domain: metaphysics/identity_resolution/epistemology
 *
 * SUMMARY:
 *   Ontological friction resolution represents a fundamental structural
 *   tension in human consciousness and social systems: the gap between the
 *   comforting illusions we maintain and the bare simplicity of integrated
 *   truth. The constraint manifests as the 'chaste fire of pain' that ignites
 *   when the 'vaporous veil of smiles' (the protective mask) is stripped away
 *   to reveal reality. This constraint exhibits all six DR types from
 *   different perspectives, making it a diagnostic exemplar for how indexical
 *   classification operates across metaphysical, psychological, and
 *   institutional domains. From the perspective of the fragmented self, the
 *   constraint appears as pure extraction (Snare): the cost of maintained
 *   illusion exceeds the cost of truth, yet neither path is costless. From
 *   the perspective of truth-seeking institutions, it appears as coordination
 *   (Rope): the friction is the functional mechanism by which knowledge
 *   integrates. From the community's view, it is a hybrid mechanism (Tangled
 *   Rope): shared illusions coordinate action while suppressing incongruent
 *   voices. From institutional custodians, it appears as degraded theater
 *   (Piton): the performance of truth-seeking without genuine friction. From
 *   temporary transition structures, it appears as scaffold: the friction is
 *   real but time-bounded and contextually supported. From the civilizational
 *   analytical view, it risks appearing as natural law (Mountain): identity
 *   integration requires friction in consciousness itself. The constraint's
 *   increasing extractiveness (0.35→0.58) and theater ratio (0.42→0.64) over
 *   the interval suggest that institutional suppression has intensified even
 *   as institutional commitment to truth-seeking has become more theatrical.
 *
 * KEY AGENTS:
 *   - Fragmented Self: Primary victim (powerless/trapped) — bears the full cost of suppressed incongruity and the chaste fire of integration
 *   - Truth Seekers: Primary beneficiary (institutional/arbitrage) — benefit from friction as a signal of genuine work; experience pain as meaningful
 *   - Community Illusion Maintainers: Secondary beneficiary (moderate/constrained) — coordinate action through shared narrative; extract suppression cost from incongruity-perceivers
 *   - Institutional Custodians: Institutional actor (institutional/constrained) — maintain performative truth-seeking while preserving comforting narratives; see own process as degraded
 *   - Truth Commissions and Scaffolding: Organized agents (organized/mobile) — temporary structures enabling transition from illusion to integration; sunset as integration completes
 *   - Epistemic Commons: Victim (powerless/trapped) — abstract collective knowledge damaged by systematic suppression of incongruent truths
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as properties of consciousness itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ontological_friction_resolution, 0.58).
domain_priors:suppression_score(ontological_friction_resolution, 0.68).
domain_priors:theater_ratio(ontological_friction_resolution, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ontological_friction_resolution, extractiveness, 0.58).
narrative_ontology:constraint_metric(ontological_friction_resolution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ontological_friction_resolution, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ontological_friction_resolution, tangled_rope).
narrative_ontology:human_readable(ontological_friction_resolution, "The Chaste Fire of Truth — Ontological Friction Resolution").
narrative_ontology:topic_domain(ontological_friction_resolution, "metaphysics/identity_resolution/epistemology").

domain_priors:requires_active_enforcement(ontological_friction_resolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, truth_seekers).
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, identity_integrators).
narrative_ontology:constraint_victim(ontological_friction_resolution, comforting_illusions).
narrative_ontology:constraint_victim(ontological_friction_resolution, fragmented_selves).
narrative_ontology:constraint_victim(ontological_friction_resolution, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRAGMENTED SELF (SNARE) — The person embedded in false narratives and comforting masks cannot exit the constraint without experiencing acute ontological pain. The illusion provides psychological protection, yet maintaining it requires active suppression of incongruent truths. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.79. Trapped: the cost of maintaining the mask exceeds the cost of removing it, yet removal triggers the chaste fire.
constraint_indexing:constraint_classification(ontological_friction_resolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMMUNITY NEGOTIATING SHARED REALITY (TANGLED ROPE) — Communities maintain collective illusions (shared myths, institutional narratives, normalized denial) that coordinate action while extracting from members who perceive incongruities. The constraint functions as coordination (shared frame) AND extraction (suppression of alternative narratives). d≈0.68, f(d)≈1.00, σ=0.9 → χ≈0.52. Constrained: members can theoretically exit but face social cost; the community benefits from the illusion's stability.
constraint_indexing:constraint_classification(ontological_friction_resolution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRUTH-SEEKING INSTITUTION (ROPE) — Scholarship, therapy, philosophy, and truth-telling systems experience the constraint as pure coordination: the friction of stripping away illusions is the functional mechanism by which they generate integrated knowledge and coherent identity. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary. The pain is the signal of legitimate work; the friction is the medium of truth.
constraint_indexing:constraint_classification(ontological_friction_resolution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL CUSTODIAN OF COMFORTABLE NARRATIVES (PITON) — Religious institutions, nationalist historiography, corporate cultures, and family systems maintain performative commitment to truth while systematically preserving comforting illusions. The ritual (confession, critical inquiry, transparency initiatives) performs truth-seeking without enabling genuine friction. theater_ratio=0.64 satisfies piton gate. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.25. Degraded: the institution once functioned as a truth-reconciliation mechanism but now functions primarily as illusion custodian.
constraint_indexing:constraint_classification(ontological_friction_resolution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TEMPORARY TRUTH DISCLOSURE APPARATUS (SCAFFOLD) — Truth commissions, restorative justice circles, therapeutic breakthroughs, and public reckoning processes function as temporary scaffolding structures that enable the transition from illusion to integration. The chaste fire is intense but time-bounded; the apparatus provides context and support. d≈0.52, f(d)≈0.70, σ=0.9 → χ≈0.32. Sunset: as individuals and communities integrate the truth, the apparatus becomes unnecessary and is dismantled or transformed. has_sunset_clause_rationale: Truth commissions conclude; therapy relationships end; public reckoning cycles through justice, integration, and resolution. The scaffold's function is to bridge the ontological gap, then dissolve.
constraint_indexing:constraint_classification(ontological_friction_resolution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the vantage of logic and ontology, the constraint appears as an immutable property: coherent identity requires integration of all truths about oneself; the friction between illusion and reality is not a contingent institutional artifact but a structural feature of consciousness itself. The chaste fire is the inevitable cost of moving from false-partitioned identity to integrated identity. However, the structural data (ε=0.58, suppression=0.68, theater=0.64) indicates this is NOT a mountain — the engine will compute this as a false summit, revealing that what appears natural is actually institutionally enforced.
constraint_indexing:constraint_classification(ontological_friction_resolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: TRUTH ITSELF AS VICTIM (SNARE) — An unusual perspective treating truth as the victim: institutional pressure to maintain comforting illusions extracts a suppression cost from reality itself. The constraint forces inconvenient truths into epistemic darkness, damaging the epistemic commons. This perspective makes visible the cost borne by abstract collective knowledge when institutions enforce illusions. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. The epistemic commons has no voice and no exit.
constraint_indexing:constraint_classification(ontological_friction_resolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ontological_friction_resolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ontological_friction_resolution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ontological_friction_resolution, TR),
    TR >= 0.70.

:- end_tests(ontological_friction_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant cost from those embedded in illusions: psychological pain, relational disruption, identity disorientation. However, the extraction is not maximal because (a) truth-seeking institutions genuinely reduce some costs through scaffolding, and (b) the integration ultimately generates benefits (coherence, authenticity, relational repair). The value reflects the asymmetry: those maintaining illusions bear concentrated costs; those practicing integration bear diffuse benefits over longer timescales. Suppression (0.68): High. Significant institutional and social barriers prevent truth-seeking: shame (cultural narratives about weakness), career risk (organizations punish honest incongruity), psychological defense (denial mechanisms evolved to protect), relational cost (truth-telling ruptures bonds built on shared illusions), institutional gatekeeping (official narratives enforced through credentialing and access control). Theater ratio (0.64): High-moderate. Many institutions perform truth-seeking (therapy, confession, critical inquiry, transparency initiatives) without enabling genuine friction. The performance creates the impression of progress while preserving core illusions. Legitimate truth-seeking reduces this ratio; institutional custodianship increases it. The rising trajectory (0.42→0.64) indicates intensifying performativity without corresponding structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from a single structural reality. The fragmented self experiences the constraint as pure extraction (Snare) — the pain is unjust suppression. The truth-seeking institution experiences it as coordination (Rope) — the pain is functional signal. The community experiences mixed dynamics (Tangled Rope) — shared illusions enable collective action while extracting from truth-perceivers. The institutional custodian sees degraded ritual (Piton) — truth-seeking performance without genuine friction. The transition apparatus sees bounded temporary support (Scaffold) — the fire is real but contextually managed and time-limited. The civilizational observer risks seeing immutable law (Mountain) — consciousness requires friction; integration requires pain. But the structural data (ε=0.58, not ≤0.25; suppression=0.68, not ≤0.05; theater=0.64, not indicating naturally emerging) reveals the mountain as false summit: institutional enforcement is creating the appearance of necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Fragmented self: victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction. No exit without bearing full cost. Truth seekers: beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; pain signals meaningful work. Community: mixed (beneficiary of coordination + victim of suppression) + constrained → d≈0.68, f(d)≈1.00. Moderate extraction; coordination provides offsetting benefit but incomplete. Institutional custodian: beneficiary of illusion maintenance + constrained → d≈0.35, f(d)≈0.32. Low extraction in self-perception, but actually constrained by degradation of original function. Scaffold: organized + mobile → d≈0.52, f(d)≈0.70. Moderate extraction; mobility means coalition can redirect resources if needed. Epistemic commons: victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction; abstract knowledge has no voice. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective is false summit; engine's detector reveals naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via six-type perspectival analysis. The temptation to misclassify this constraint arises from conflating two structurally distinct claims: (A) 'Identity integration requires painful confrontation with incongruence' (this would be Mountain: ε≤0.25), and (B) 'Institutions systematically suppress truth to maintain control, using pain as a mechanism' (this is Tangled Rope: ε≈0.58, requires active enforcement, has beneficiaries and victims). The constraint story disambiguates by showing that the analytical observer's mountain is a FALSE SUMMIT — a naturalization of what is actually institutional extraction. The raw structural data (extractiveness 0.58, suppression 0.68, theater 0.64) cannot sustain a mountain classification. If pain were truly intrinsic to consciousness, we would expect: (i) universal inevitability (but therapeutic and transitional contexts reduce pain severity without reducing integration success), (ii) minimal suppression (but institutions actively enforce narrative consistency), (iii) low theater (but truth-seeking performance is widespread). The falseness of the summit reveals the institutional nature of the constraint. However, the omega variables (pain_necessity_threshold, alternative_disclosure_pathways, shared_illusion_stability, consciousness_bootstrap_requirement) leave open the possibility that some core ontological friction IS natural law, while the EXCESS pain and ENFORCED suppression are institutional artifact. This mixed possibility — real ontological friction overlaid with extractive institutional enforcement — is precisely what Tangled Rope captures: genuine coordination function (managing legitimate identity integration) + asymmetric extraction (institutional suppression of incongruity costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pain_necessity_threshold,
    'Is the chaste fire of pain a necessary feature of ontological friction, or a contingent artifact of institutional suppression?',
    'Comparative analysis: individuals/communities that transition from illusion to integration with/without institutional scaffolding; measurement of pain intensity vs. institutional suppression level vs. transition success rates',
    'If pain is necessary: the constraint approaches Mountain (immutable feature of consciousness). If pain is artifact: the constraint is Tangled Rope with extractive institution-imposed suffering superadded onto legitimate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pain_necessity_threshold, conceptual, 'Whether pain is intrinsic to ontological integration or institutional artifact').

omega_variable(
    alternative_disclosure_pathways,
    'Do gradual, partial truth disclosures without institutional mediation produce equivalent integration outcomes to intensive institutional friction?',
    'Longitudinal tracking of identity integration outcomes for self-directed vs. institutionally-guided truth work; measurement of psychological stability, relational repair, and long-term friction recurrence',
    'If equivalent: scaffold institution is unnecessary extraction (Snare). If inferior: institution provides genuine coordination value (Rope or Tangled Rope). If superior: the institution is actually an artifact and truth-seeking benefits from freedom from supervision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_disclosure_pathways, empirical, 'Whether institutional mediation improves truth integration outcomes').

omega_variable(
    shared_illusion_stability,
    'Do communities that maintain shared comforting illusions (despite internal incongruities) exhibit greater stability and flourishing than communities committed to integrated truth?',
    'Comparative study of social stability, mental health outcomes, longevity, and flourishing metrics across cohorts: high-illusion-maintenance (tight shared narrative) vs. high-truth-integration (distributed incongruity management)',
    'If illusion-maintaining is superior: the constraint''s suppression is functionally justified (Rope). If equivalent or inferior: suppression is pure extraction (Snare or Tangled Rope). If inferior but universal: the constraint is a tragedy (everyone pays the cost of maintained illusions).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shared_illusion_stability, empirical, 'Whether illusion-maintenance improves community outcomes vs. truth integration').

omega_variable(
    consciousness_bootstrap_requirement,
    'Is ontological friction logically required to bootstrap consciousness from unconscious unity into reflective awareness?',
    'Philosophical analysis: can integration occur without friction? Empirical: are there documented cases of identity integration without pain or institutional suppression? Comparative psychology: do non-human animals require equivalent friction to develop coherence?',
    'If logically required: Mountain (immutable). If empirically rare but possible: Snare being mistaken for necessity. If artifact of human institutional history: Tangled Rope maintainable through reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consciousness_bootstrap_requirement, conceptual, 'Whether ontological friction is logically necessary for consciousness integration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ontological_friction_resolution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ofr_tr_t0, ontological_friction_resolution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ofr_tr_t10, ontological_friction_resolution, theater_ratio, 10, 0.53).
narrative_ontology:measurement(ofr_tr_t20, ontological_friction_resolution, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(ofr_be_t0, ontological_friction_resolution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ofr_be_t10, ontological_friction_resolution, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ofr_be_t20, ontological_friction_resolution, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ontological_friction_resolution, information_standard).
narrative_ontology:affects_constraint(ontological_friction_resolution, institutional_narrative_control).
narrative_ontology:affects_constraint(ontological_friction_resolution, psychological_fragmentation).
narrative_ontology:affects_constraint(ontological_friction_resolution, epistemic_commons_contamination).

% DUAL FORMULATION NOTE:
% Ontological friction resolution decomposes into multiple constraint stories depending on observational focus. The present story focuses on the institutional-individual interface and the identity integration process. Upstream constraints include the natural law of consciousness (if it exists: constraint_consciousness_integration_necessity), and the institutional mechanisms of narrative control (constraint_institutional_narrative_control). Downstream constraints include the systemic effects on epistemic commons and the psychology of fragmented identity (constraint_psychological_fragmentation, constraint_epistemic_commons_contamination). The present story's ε=0.58 reflects institutional extraction superimposed on legitimate ontological work; if the institutional extraction were removed, ε would decline toward 0.25-0.35 (reflecting the natural friction component alone). The ambiguity is captured in the omega variables, not in the JSON structure — each story maintains a single stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ontological_friction_resolution, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
