% ============================================================================
% CONSTRAINT STORY: international_law_instrumentalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_law_instrumentalization, []).

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
 *   constraint_id: international_law_instrumentalization
 *   human_readable: International Law Instrumentalization in Territorial Sovereignty Disputes
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   The instrumentalization of international law in territorial sovereignty
 *   disputes represents a structural constraint where the same legal
 *   framework that claims to adjudicate legitimate claims becomes a mechanism
 *   for entrenching power-determined outcomes. The Israeli-Palestinian
 *   territorial dispute exemplifies this: both polities invoke international
 *   law principles (self-determination, non-interference in internal affairs,
 *   territorial integrity, human rights protections, humanitarian law) but
 *   international law coherence fails when these principles contradict each
 *   other in application. The constraint operates at multiple levels: (1) the
 *   legal doctrines are formally incoherent — they coexist in contradiction;
 *   (2) the enforcement mechanism selectively applies law to outcomes already
 *   determined by military power; (3) the rival sovereignty claim becomes
 *   structurally trapped by a legal framework designed to privilege de facto
 *   control and established statehood; (4) the international legal system
 *   maintains legitimacy through performative invocation of principles it
 *   cannot coherently apply. The measurements show accumulating
 *   extractiveness (0.35 → 0.72 over 75 years) and rising theater ratio (0.45
 *   → 0.81), indicating that the constraint has evolved from ad hoc military
 *   administration into a heavily legalized, institutionalized occupation
 *   framework. The suppression requirement has correspondingly increased
 *   (0.48 → 0.72) as legal mechanisms replace crude force, making the
 *   constraint simultaneously more sophisticated and more durable. This is a
 *   snare that has become harder to escape precisely because it has become
 *   more juridically articulated.
 *
 * KEY AGENTS:
 *   - International Law Wielding State (Institutional/Arbitrage): Derives legitimacy and deterrence benefits from invoking law; can arbitrage between doctrines; sees constraint as coordination mechanism (Rope perspective)
 *   - Dispossessed Population (Powerless/Trapped): Bears full cost of legal instrumentalization; trapped by military occupation, legal disenfranchisement, territorial control, and restrictions on exit; no alternatives; maximum extraction (Snare perspective)
 *   - Rival Sovereignty Claim (Powerless/Trapped, Generational): Structurally trapped by international legal architecture that privileges de facto control; cannot build state without territory, cannot secure territory against entrenched control; suppressed by the same legal framework invoked against it (Snare perspective)
 *   - International Legal System (Institutional/Arbitrage): Maintains institutional inertia by performing coherence while applying law selectively; benefits from legitimation function while escaping accountability for contradictions (Piton perspective)
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing power conflict as immutable law of political nature, obscuring the contingent institutional arrangements that benefit specific actors (False Summit perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_law_instrumentalization, 0.68).
domain_priors:suppression_score(international_law_instrumentalization, 0.72).
domain_priors:theater_ratio(international_law_instrumentalization, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_law_instrumentalization, extractiveness, 0.68).
narrative_ontology:constraint_metric(international_law_instrumentalization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(international_law_instrumentalization, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_law_instrumentalization, snare).
narrative_ontology:human_readable(international_law_instrumentalization, "International Law Instrumentalization in Territorial Sovereignty Disputes").
narrative_ontology:topic_domain(international_law_instrumentalization, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(international_law_instrumentalization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(international_law_instrumentalization, '14e90171-2ec6-41f3-8215-998483d05866').
narrative_ontology:cs_kernel_codification('14e90171-2ec6-41f3-8215-998483d05866', formalized).
narrative_ontology:cs_authority_grounding('14e90171-2ec6-41f3-8215-998483d05866', extraction).
narrative_ontology:cs_interpretation_layer_present('14e90171-2ec6-41f3-8215-998483d05866').
narrative_ontology:cs_reading_relation('14e90171-2ec6-41f3-8215-998483d05866', international_law_legitimate_adjudication, forecloses).
narrative_ontology:cs_reading_relation('14e90171-2ec6-41f3-8215-998483d05866', territorial_legitimacy_natural_law, coexists_with).
narrative_ontology:cs_axiom('14e90171-2ec6-41f3-8215-998483d05866', foundational, law_instrumentalized_for_power).
narrative_ontology:cs_axiom_status(law_instrumentalized_for_power, holdable).
narrative_ontology:cs_axiom_grounding('14e90171-2ec6-41f3-8215-998483d05866', law_instrumentalized_for_power, empirically_contingent).
narrative_ontology:cs_axiom('14e90171-2ec6-41f3-8215-998483d05866', foundational, international_law_coherence_failure).
narrative_ontology:cs_axiom_status(international_law_coherence_failure, holdable).
narrative_ontology:cs_axiom_grounding('14e90171-2ec6-41f3-8215-998483d05866', international_law_coherence_failure, empirically_contingent).
narrative_ontology:cs_axiom('14e90171-2ec6-41f3-8215-998483d05866', secondary, de_facto_control_privileges_entrenchment).
narrative_ontology:cs_axiom_status(de_facto_control_privileges_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('14e90171-2ec6-41f3-8215-998483d05866', de_facto_control_privileges_entrenchment, conventional).
narrative_ontology:cs_reference_frame('14e90171-2ec6-41f3-8215-998483d05866', international_law_neutral_adjudication).
narrative_ontology:cs_drift_state('14e90171-2ec6-41f3-8215-998483d05866', contemporary_instrumentalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('14e90171-2ec6-41f3-8215-998483d05866', '2026-02-26T14:23:47Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_law_instrumentalization, international_law_wielding_state).
narrative_ontology:constraint_beneficiary(international_law_instrumentalization, military_occupying_apparatus).
narrative_ontology:constraint_victim(international_law_instrumentalization, dispossessed_population).
narrative_ontology:constraint_victim(international_law_instrumentalization, rival_sovereignty_claim).
narrative_ontology:constraint_victim(international_law_instrumentalization, international_law_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED POPULATION (SNARE) — Trapped by military occupation, legal disenfranchisement, and territorial control. International law is weaponized against exit: claims of legal invalidity are met with enforcement mechanisms (permit systems, settlement law, transfer restrictions). No alternatives available; maximum suppression and extraction. The victim experiences this as pure constraint with no coordination benefit.
constraint_indexing:constraint_classification(international_law_instrumentalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RIVAL SOVEREIGNTY CLAIM (SNARE, GENERATIONAL) — Trapped not by direct military force but by international legal architecture that privileges de facto control and first-mover advantage. Cannot build state apparatus without territorial base; cannot secure territorial base without recognition; cannot secure recognition against entrenched control. High suppression from legal instrumentalization: the same legal framework that legitimates occupation delegitimizes counter-claims.
constraint_indexing:constraint_classification(international_law_instrumentalization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL LAW WIELDING STATE (ROPE) — Experiences the legal framework as coordination: invoking sovereignty, self-defense, international law principles produces legitimacy and deters intervention. The state benefits from the rules (Rule of Law, non-interference, territorial integrity for established states) and can arbitrage between legal doctrines (invoking humanitarian law, security doctrine, historical claims). Net beneficiary. The constraint functions as coordination from this perspective.
constraint_indexing:constraint_classification(international_law_instrumentalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL SYSTEM (PITON) — The UN charter, Geneva Conventions, and customary law framework are largely theatrical from the system's own viewpoint. The legal doctrines coexist in formal contradiction (self-determination vs. territorial integrity vs. non-intervention) and are selectively invoked to legitimize whatever outcome is already enforced by power. Theater ratio (0.81) captures the performative nature: legal argument proceeds as if incoherent principles can be reconciled, while power determines which principle applies to which case. Piton classification: the system is maintained through institutional inertia and ceremonial invocation, not because it functions coherently.
constraint_indexing:constraint_classification(international_law_instrumentalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (FALSE SUMMIT) — From the analytical/civilizational perspective, territorial conflict is an immutable feature of political organization — scarcity of land, irreducible conflicts of interest, and the zero-sum nature of territory make disputes inevitable and irresolvable. This perspective treats the constraint as a law of political nature. However, the structural data reveals beneficiaries (the state wielding law instrumentally), victims (the dispossessed population), and active enforcement mechanisms. The engine will classify this as a false summit: what appears to be natural law is actually a contingent institutional arrangement (international law's selective enforcement) that benefits identifiable agents.
constraint_indexing:constraint_classification(international_law_instrumentalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_law_instrumentalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_law_instrumentalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_law_instrumentalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_law_instrumentalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_law_instrumentalization, TR),
    TR >= 0.70.

:- end_tests(international_law_instrumentalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantially from the dispossessed population through legal mechanisms: property law enforces territorial transfer, administrative law restricts movement, military law enables detention without trial, and international law delegitimizes counter-claims. The rival sovereignty claim is trapped by legal architecture that makes legitimate state-building impossible. The measuring state extracts legitimacy, deterrence against international intervention, and freedom to expand control. The measurement trajectory (0.35 → 0.72 over 75 years) shows extraction accumulating as legal institutions entrench: what began as military administration has become a sophisticated legal occupation structure. Suppression (0.72): High. Multiple mechanisms prevent exit: military occupation, settlement law that restricts Palestinian property acquisition, permit systems that control movement, legal restrictions on governance, and international law doctrines that invalidate counter-claims. Suppression has increased over time (0.48 → 0.72) as legal mechanisms replace crude force, making the constraint more durable. Theater ratio (0.81): Very high. International legal argument is substantially performative. Legal doctrines coexist in formal contradiction (territorial integrity vs self-determination vs non-interference), and which principle applies to which case is determined by power, not by legal logic. The UN system invokes law while enabling outcomes against law; legal scholars debate irresolvable doctrinal conflicts while military occupation continues. The increasing theater ratio (0.45 → 0.81) reflects growing sophistication of legal performance: as the constraint has become more institutionalized, legal legitimation has become more elaborate while actual legal coherence has not improved.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range: the wielding state sees coordination (Rope) — law functions as intended, protecting legitimate state interests. The rival claim sees pure extraction (Snare) — law is weaponized against recognition. The dispossessed population sees pure extraction (Snare) — legal mechanisms enforce dispossession. The international system sees its own degradation (Piton) — law is invoked performatively while coherence collapses. The analytical observer risks a false summit (Mountain) — treating territorial conflict as inevitable natural law, obscuring instrumentalization. The perspectival gaps reveal that this is not a dispute within a coherent legal framework but a conflict between incoherent kernels: the wielding state reads international law as legitimating its position; the rival claim reads the same law as delegitimating it; the system cannot adjudicate between them because the foundational principles contradict each other in application.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality d is determined by the agent's structural relationship to the constraint. The wielding state is a beneficiary with arbitrage options (low d → negative χ) — the constraint functions for them as legitimacy. The dispossessed population is a victim with no exit (high d → high χ) — maximum extraction. The rival claim is a victim with no path to recognition (very high d, close to 1.0 → maximum χ) — trapped by the legal framework itself. The international system benefits from maintaining legitimacy (institutional/arbitrage, low d) but at cost of coherence (it must pretend to apply law coherently while selectively enforcing it). The analytical observer is observational (analytical/analytical) but risks naturalizing power as law. The gap in directionality values explains why perspectives classify differently: the beneficiary experiences the constraint as functional coordination; the victim experiences it as pure extraction; the system experiences it as performative maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This is a snare, not a tangled rope. The distinction hinges on whether genuine coordination exists alongside extraction. While international law does coordinate state behavior in some domains (trade, environmental protection, maritime law), in territorial sovereignty disputes it functions as pure extraction: the legal framework produces no coordination benefit for the trapped parties (dispossessed population, rival claim). The coordination that appears to exist (both parties invoke law) is theatrical — the law is invoked because it carries legitimacy, but it does not adjudicate disputes fairly. The beneficiary (wielding state) experiences coordination (Rope perspective) because law functions as intended for them. The victim (dispossessed population) experiences pure extraction (Snare perspective) because law blocks legitimate self-determination. The measurement trajectory (rising extractiveness, rising theater, rising suppression requirement) shows the constraint hardening into a snare: as legal mechanisms entrench, the extractive function becomes more durable and the theoretical possibility of legal resolution becomes more remote. The false summit (mountain perspective treating territorial conflict as natural law) is resolved by identifying beneficiaries, victims, and enforcement mechanisms — all contingent institutional features, not laws of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_coherence,
    'Is territorial legitimacy ONE kernel read two ways (Israeli and Palestinian readings of the same foundational claim), or TWO incoherent kernels with no shared substrate?',
    'Textual analysis of foundational claims: do both readings invoke a common authority (e.g., UN resolutions, international law principles)? If yes, they share a kernel; if foundational authorities are mutually exclusive or internally contradictory across readings, they are incoherent kernels. Examine whether each reading acknowledges the other''s premises or treats them as invalid a priori.',
    'If one kernel: the constraint is a reading divergence problem; both claims are structurally valid within their frameworks; resolution requires reconciling interpretations. If two incoherent kernels: no shared epistemic ground exists; the constraint is a power struggle masked as legal dispute; international law becomes an instrumentalization mechanism rather than a truth-seeking apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_coherence, conceptual, 'Whether territorial legitimacy is one contested kernel or two incoherent claims').

omega_variable(
    international_law_as_cover_story,
    'Does international law function as a genuine constraint on state behavior, or as a post-hoc legitimation cover story for power-determined outcomes?',
    'Historical case analysis: do states reverse course when international legal positions change? When international law contradicts national interest, do states comply or invoke doctrinal escape clauses? Compare enforcement: are legal violations by powerful states reversed vs violations by weak states prosecuted? Statistical likelihood of legal victory correlating with military power.',
    'If genuine constraint: international law deterrence is real; legal arguments can shift outcomes; the snare classification is exaggerated. If cover story: law is purely instrumental; snare classification is accurate; international law becomes part of the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_as_cover_story, empirical, 'Whether international law constrains or legitimizes power').

omega_variable(
    dual_reading_foreclosure,
    'Do the Israeli and Palestinian readings of territorial legitimacy logically foreclose each other, or do they coexist as incompatible claims held by different parties?',
    'Formal analysis of foundational axioms in each reading: does accepting one reading require denying the other''s core premise (foreclosure), or can both be held simultaneously by different polities without logical contradiction (coexistence under disagreement)? Test: can a neutral jurist acknowledge both readings as internally coherent, even while rejecting one?',
    'If foreclosure: one reading must be abandoned; resolution requires vindication of one framework and rejection of the other; international law becomes the adjudication mechanism. If coexistence: both readings are structurally defensible; the conflict is not resolvable through legal argument; the snare becomes a permanent feature of the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_reading_foreclosure, conceptual, 'Whether readings logically foreclose each other or coexist').

omega_variable(
    extraction_accumulation_mechanism,
    'Is the extractiveness (0.68) static, or is it accumulating over time as legal frameworks entrench de facto control?',
    'Longitudinal analysis of settlement expansion, legal restrictions on Palestinian movement/property/governance, and entrenchment of occupation mechanisms: does each decade show increased legal institutionalization of occupation status quo? Measurement: rate of change in extractiveness (are measurements showing upward trajectory?).',
    'If static: this is a stable institutional equilibrium (structural snare but not degrading). If accumulating: the constraint is undergoing institutional hardening — what began as temporary military administration is becoming permanent legal structure — shifting from snare toward totalizing subjugation. The measurement trajectory matters: rising extractiveness suggests the snare is not temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_accumulation_mechanism, empirical, 'Whether extraction is accumulating over time').

omega_variable(
    rival_sovereignty_recognition_asymmetry,
    'Is the rival sovereignty claim structurally unrecognizable, or is it strategically unrecognized by the international community due to power dynamics?',
    'Comparative analysis: what criteria would be sufficient for rival claim recognition? Are those criteria being withheld indefinitely, or are they impossible to meet (structural unrecognizability)? Compare to other independence movements (East Timor, Kosovo, South Sudan) — what did they achieve that the rival claim has not? Is the difference technical/institutional or power-based?',
    'If structurally unrecognizable: the rival claim cannot succeed within international law; the constraint is about impossible transformation. If strategically unrecognized: the rival claim could theoretically succeed, but power interests prevent recognition; the constraint is instrumentalization masquerading as law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rival_sovereignty_recognition_asymmetry, empirical, 'Whether rival sovereignty is structurally impossible or strategically blocked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_law_instrumentalization, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intl_law_theater_1948, international_law_instrumentalization, theater_ratio, 0, 0.45).
narrative_ontology:measurement(intl_law_theater_1973, international_law_instrumentalization, theater_ratio, 25, 0.62).
narrative_ontology:measurement(intl_law_theater_1998, international_law_instrumentalization, theater_ratio, 50, 0.75).
narrative_ontology:measurement(intl_law_theater_2023, international_law_instrumentalization, theater_ratio, 75, 0.81).

% Extraction over time
narrative_ontology:measurement(intl_law_extract_1948, international_law_instrumentalization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(intl_law_extract_1973, international_law_instrumentalization, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(intl_law_extract_1998, international_law_instrumentalization, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(intl_law_extract_2023, international_law_instrumentalization, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(intl_law_supp_1948, international_law_instrumentalization, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(intl_law_supp_1973, international_law_instrumentalization, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(intl_law_supp_1998, international_law_instrumentalization, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(intl_law_supp_2023, international_law_instrumentalization, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_law_instrumentalization, enforcement_mechanism).
narrative_ontology:affects_constraint(international_law_instrumentalization, settlement_expansion_legal_framework).
narrative_ontology:affects_constraint(international_law_instrumentalization, refugee_return_right_negation).
narrative_ontology:affects_constraint(international_law_instrumentalization, rival_state_institutional_capacity).
narrative_ontology:affects_constraint(international_law_instrumentalization, international_court_jurisdiction_limits).

% DUAL FORMULATION NOTE:
% International law instrumentalization is part of a constraint family including: (1) settlement_expansion_legal_framework (ε=0.72, Snare) — legal mechanisms enabling territorial expansion; (2) refugee_return_right_negation (ε=0.65, Snare) — legal doctrine negating return rights; (3) rival_state_institutional_capacity (ε=0.58, Tangled Rope) — mixed coordination/extraction in building governance without territory; (4) international_court_jurisdiction_limits (ε=0.42, Tangled Rope) — mix of coordinating principle and limiting enforcement. Each story has its own ε and perspectives; they are linked by shared beneficiary (wielding state) and common victim (dispossessed population). This story documents the overarching legal instrumentalization mechanism; downstream stories show specific applications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_law_instrumentalization, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
