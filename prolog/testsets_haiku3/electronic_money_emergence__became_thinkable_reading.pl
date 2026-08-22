% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Digital Money Emergence (Conceptual-Thinkability Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'electronic_money_emergence.' This reading asserts that digital money
 *   emerged when the conceptual possibility became technically and socially
 *   thinkable—prior to institutional measurement by central banks or formal
 *   adoption by regulatory frameworks. The referent is the standing
 *   arrangement of monetary innovation history as understood through this
 *   frame: digital money's emergence is dated to the conceptualization phase
 *   (1960s–1980s theoretical work by monetary theorists and early
 *   technologists), NOT to later institutional measurement (M4/M5 statistical
 *   adoption in the 1990s–2000s) or to the first institutional holder of
 *   dematerialized currency. Extractiveness is moderate (0.31 at interval
 *   end) because the reading distributes narrative authority to beneficiaries
 *   (theorists, technologists) and away from institutional
 *   measurement-setters; suppression is low (0.18) because no party is
 *   actively excluded from this framing—it is a difference of historical
 *   interpretation, not institutional exclusion. Theater is moderate (0.22)
 *   because much of the ongoing rhetorical work of this reading involves
 *   reframing institutional adoption as 'catching up' to prior conceptual
 *   work, a performative act overlaying the technical/institutional facts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.31).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.18).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Digital Money Emergence (Conceptual-Thinkability Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, 'cd810da9-273b-4665-9472-c26be4d93038').
narrative_ontology:cs_kernel_codification('cd810da9-273b-4665-9472-c26be4d93038', distributed).
narrative_ontology:cs_authority_grounding('cd810da9-273b-4665-9472-c26be4d93038', distributed).
narrative_ontology:cs_reading_relation('cd810da9-273b-4665-9472-c26be4d93038', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd810da9-273b-4665-9472-c26be4d93038', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('cd810da9-273b-4665-9472-c26be4d93038', foundational, conceptualization_temporally_prior_to_measurement).
narrative_ontology:cs_axiom_status(conceptualization_temporally_prior_to_measurement, holdable).
narrative_ontology:cs_axiom_grounding('cd810da9-273b-4665-9472-c26be4d93038', conceptualization_temporally_prior_to_measurement, instrumental).
narrative_ontology:cs_axiom('cd810da9-273b-4665-9472-c26be4d93038', foundational, imagination_grounding_institutional_reality).
narrative_ontology:cs_axiom_status(imagination_grounding_institutional_reality, holdable).
narrative_ontology:cs_axiom_grounding('cd810da9-273b-4665-9472-c26be4d93038', imagination_grounding_institutional_reality, deontological).
narrative_ontology:cs_reference_frame('cd810da9-273b-4665-9472-c26be4d93038', conceptual_thinkability_as_reality_threshold).
narrative_ontology:cs_drift_state('cd810da9-273b-4665-9472-c26be4d93038', contemporary_cbdc_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cd810da9-273b-4665-9472-c26be4d93038', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, conceptual_innovators).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, financial_technologists).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, academic_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, central_banks).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, commercial_banks).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, monetary_aggregation_statisticians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theorists, technologists, and visionaries who articulate the possibility of dematerialized currency before institutional systems implement it. They benefit from narrative primacy: their conception of 'when digital money emerged' becomes the canonical reference point against which institutions measure themselves. Their work sets the frame institutions later adopt.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, conceptual_innovators, beneficiary,
    moderate, generational, mobile, global).

% Engineers and systems architects who translate conceptual possibility into technical artifact. They benefit from a reading that timestamps emergence at conceptualization: it establishes their field (digital currency design) as the origin point of digital money, shifting legitimacy away from institutional adoption and toward technical innovation. Their intellectual property claims and consulting expertise are grounded in this framing.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, financial_technologists, beneficiary,
    powerful, biographical, arbitrage, global).

% Economists, historians of technology, and monetary theorists who study emergence processes. They benefit from a reading that privileges the concept-to-technical phase, not institutional adoption: it creates a research agenda around cognitive and technical preconditions, opens publication venues, and establishes theoretical primacy over administrative measurement.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, academic_theorists, beneficiary,
    moderate, generational, mobile, global).

% Monetary authorities tasked with measuring, regulating, and stabilizing the money supply. Under this reading, they are positioned as late-stage adopters of a conceptual possibility that emerged elsewhere, decades prior. They bear the cost of retrofitting measurement categories (M4, M5 monetary aggregates) to accommodate a phenomenon they did not originate, and they lose narrative authority over defining money's boundaries.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, central_banks, payer,
    institutional, generational, constrained, national).

% Financial intermediaries that implemented digital payment systems and held dematerialized deposits. Under this reading, their actual operational deployment of digital money is secondary to the prior conceptualization. Their practical innovations in clearing, settlement, and ledger management are reframed as mere execution of an already-thinkable idea, diminishing their claim to have 'created' digital money.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, commercial_banks, payer,
    powerful, biographical, constrained, global).

% Data collectors and national accounting bodies that measure money supply through M0, M1, M2, M3, M4, M5 categories. This reading positions their measurement work as a retroactive attempt to fit conceptual possibility into quantitative fact. They bear the cost of category collapse and redefinition when the conceptual/technical distinction they are trying to measure turns out to be less clear than the concept promised.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, monetary_aggregation_statisticians, payer,
    moderate, biographical, constrained, national).

% Cryptocurrency advocates, alternative-money theorists, and decentralization proponents who dispute any single reading's authority. They argue that digital money emerged in multiple, incommensurable ways (cypherpunk theory, peer-to-peer protocol invention, regulatory evasion narratives) and that privileging conceptual thinkability over institutional measurement is itself a choice to frame digital money as a monetary-theoretic problem rather than a technology-societal problem. They are excluded from the official emergence narrative.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, regulation_skeptics, excluded,
    moderate, biographical, mobile, global).

% Analytical seat examining the reading's coherence and evidence base. Observes that the 'became thinkable' claim depends on identifying specific texts, conversations, or institutional contexts where the concept crystallized, and that the lag between conceptualization and measurement admits multiple interpretations depending on which concept is being tracked.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, economic_historian_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a temporal framing for understanding monetary innovation: the coordination problem solved is the need for a coherent narrative about WHEN money transitioned from physical to digital, so that historians, theorists, and policymakers can reference a common origin point and measure institutional drift from it.
% TRANSFER_FUNCTION: Transfers narrative authority over money's definition from central banks (institutional measurement) to theorists and technologists (conceptual innovation). Moves credit and primacy from those who deployed systems to those who imagined them first.
% ABSENT_VOICES: Cryptocurrency/decentralization advocates who argue emergence is plural and tech-centric, not singular and theory-centric. Alternative-money theorists who frame digital money's emergence around regulatory evasion, not conceptual evolution. Non-Western monetary innovators whose digital money concepts developed independently of Anglo-American financial theory and thus vanish under a 'became thinkable in Western discourse' reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the emergence narrative would shift to institutional-first framings (first_held_reading: when central banks and commercial banks first held digital currency) or measurement-artifact framings (m4_m5_collapse_reading: when statistical categories forced a reckoning). The temporal priority claimed by this reading—conceptualization before measurement—would cease to structure historical analysis, and the origin story of digital money would reorganize around a different threshold.
% FOUNDING_PROBLEM: The founding problem: how do we mark the moment when a monetary form ceases to be an imaginary possibility and becomes a thinkable reality? Early monetary theorists (circa 1960s–1980s) faced a practical problem: electronic payment systems and dematerialized accounting were deployed in practice, but the conceptual language to describe them as a distinct monetary category did not yet exist. The founding reading asserts that digital money's emergence should be dated to when the conceptual language—the thinkable idea of dematerialized currency—crystallized, not when central banks could measure it.
% FOUNDING_PROBLEM_CORROBORATION: Theorists in monetary economics and history of technology (who benefit from this reading's conceptual primacy) attest the founding problem was live in the 1960s–1980s. However, central banks and regulatory authorities attest that the problem is now resolved by institutional adoption of measurement categories (M4, M5, CBDC frameworks): money is what the institutions measure and deploy, and academic conceptualization is post-hoc justification. Economic historians and scientists outside the benefiting parties document that by 2020–2025, institutional innovation in digital-currency frameworks proceeds from regulatory initiatives and technical deployment, not from prior academic theory—the founding problem's driving motivation has been absorbed by institutional practice, making the conceptual-priority reading an ex-post rationalization rather than a generative framework.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).
:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises gradually from 0.12 (early diffusion) to 0.31 (established frame) because conceptual-thinkability framing grows in influence as more institutions adopt digital payment systems and retroactively justify them as 'implementing a prior vision.' The frame extracts narrative authority from later institutional actors. Suppression is low (0.05 initially, 0.18 at end) because no active coercion is required to maintain this reading—it lives in academic and technical discourse and does not directly exclude other actors from material participation. Theater rises from 0.05 to 0.22 because the reading requires continuous rhetorical work to sustain the claim that 'conceptualization is prior to and more real than measurement'—a performative assertion, not a material fact. The plateau at t=30 onward reflects saturation: once the framing is established in academic and technical discourse, further extraction requires only maintenance, not accelerating diffusion. Measurement series authored on one shared time grid (t=0,10,20,30,40,50) so all metrics are present at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (theorist/technologist), this reading is obviously true: innovation in any domain requires prior imagination, so digital money's conceptual groundwork precedes institutional implementation. From the payer seat (central banker), the reading is backward: central banks did not wait for academic theorists to 'conceive' digital money—they built electronic payment systems and then asked economists to explain what they had created. From the alternative-money seat (excluded), the reading is Anglo-American and theory-centric, erasing multiple parallel emergence processes (cypherpunk protocols, peer-to-peer innovation, regulatory-evasion narratives) that were not 'thinkable' in mainstream monetary theory but emerged anyway. The engine should compute these divergences from the power atoms and exit options: central banks (institutional) have different time horizons (generational) and different constraints than academic theorists (moderate power, mobile exit), leading to different effective extraction values for the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Conceptual innovators (theorists, technologists) sit as beneficiaries (d near 0.0): they gain narrative authority and epistemic primacy from a reading that privileges imagination over measurement. Central banks and monetary statisticians sit as payers (d near 1.0): they bear the cost of accepting that measurement is secondary to prior conceptualization, losing authority over defining money's boundaries. Commercial banks are constrained payers (d mid-range): they deployed systems but are reframed as executing a prior vision rather than originating the monetary form. Regulation skeptics and alternative-money advocates are excluded (no formal role in this reading's framework), though their exclusion is soft—they are outside the mainstream monetary-theory discussion rather than explicitly barred. The engine will compute per-seat directionality from these structural relationships; divergence between payer and beneficiary seats is expected and described by the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how do we mark emergence?) was live in the 1960s–1980s when monetary theorists worked to incorporate electronic transactions into aggregate money definitions. By 2000–2010, the problem was contested: central banks had moved to measurement-first definitions (M4, M5 statistical categories), making institutional adoption the de facto reference point, while academic theorists maintained that prior conceptualization was logically prior. By 2020–2025, the problem is dead as far as mainstream monetary policy is concerned: central banks define money by what they measure, and institutional innovation in digital-currency frameworks (CBDC, blockchain, tokenization) proceeds from regulatory initiatives, not from prior academic theory. The reading persists, but its founding problem is obsolete. This is a mandatrophy case: the constraint (the 'became thinkable' reading) persists in academic discourse and history-of-technology scholarship, but the institutional coordination problem it claimed to solve has been overtaken by institutional measurement regimes. The constraint's persistence is now mostly performative: theorists assert conceptual primacy, but central banks ignore the assertion and organize money's real boundaries through measurement categories. Theater ratio rising from 0.05 to 0.22 models this: as the reading's founding problem dies, more of its maintenance work is rhetorical/performative rather than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concept_threshold_ambiguity,
    'At what specific moment or text or conversation did digital money ''become thinkable''? Is there a canonical threshold, or is the thinkability process so gradual that dating it is arbitrary?',
    'Intellectual history: identify the first published work or institutional discussion that clearly articulates ''dematerialized currency as a distinct monetary category'' (vs. earlier payment-automation discussions that do not name a new money form). Compare multiple origin narratives from different disciplines and geographies to assess whether a consensus threshold exists.',
    'If a sharp threshold exists (e.g., a specific 1968 paper, a 1972 conference), the reading''s claim to temporal priority is strengthened. If thinkability is gradual and distributed (multiple parallel innovations, no consensus origin), the reading''s implicit claim to a singular emergence moment collapses and the reading must reframe around ''diffusion'' rather than ''emergence,'' weakening the extraction of narrative authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(concept_threshold_ambiguity, empirical, 'Whether digital-money thinkability has a datable threshold or is a gradual, distributed, non-datable process.').

omega_variable(
    reading_kernel_foreclosure,
    'Does the ''became thinkable'' reading logically foreclose either the ''first_held'' reading or the ''m4_m5_collapse'' reading within a single coherent framework?',
    'Logical analysis of the three readings'' core premises: does asserting ''emergence = thinkability'' make it impossible to also assert ''emergence = first holding'' or ''emergence = measurement artifact'' without contradiction? Or can a single analyst hold that emergence is (a) conceptually prior AND (b) materially instantiated when first held AND (c) retroactively defined by measurement categories?',
    'If the readings genuinely foreclose each other, the kernel is more like a single-passage theorem (exactly one can be true). If they coexist without logical contradiction, they are distinct perspectives on the same event, and the contest is about which framing is more useful or prior, not about truth conditions. This maps directly to cs_structure.reading_relations: the difference between ''forecloses'' and ''coexists_with'' hinges on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Logical structure of the three readings: do they foreclose or coexist?').

omega_variable(
    western_theory_centric_bias,
    'Is the ''became thinkable'' reading inherently Anglo-American and Western-monetary-theory-centric, or can the same principle apply to parallel digital-money innovations that emerged outside mainstream academic discourse?',
    'Historiography of digital money in non-Western contexts (e.g., M-Pesa in Kenya, mobile-money in the Philippines, digital payment systems in China): were these systems preceded by prior academic conceptualization, or did they emerge through pragmatic innovation with later theoretical rationalization? If pragmatic emergence without prior theory is found, does the reading''s principle of ''conceptualization precedes measurement'' apply globally or only to Western institutional science?',
    'If the reading is genuinely universal (conceptualization always precedes implementation), it gains analytical power and loses the ''excluded voice'' problem: non-Western systems would also be reframed as executions of prior visions. If the reading is theory-centric and parochial, it loses universality and becomes a reading of Western monetary science, not of digital money per se, strengthening the case for the ''excluded voices'' (regulation skeptics, alternative-money theorists) who argue emergence is plural and context-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(western_theory_centric_bias, conceptual, 'Universality of the ''conceptualization precedes measurement'' principle across non-Western digital-money innovations.').

omega_variable(
    committer_frame__reading_distinction,
    'This story instantiates ONE READING of the ''electronic_money_emergence'' kernel. What is the difference between ''this is a reading'' and ''this is a true claim about when digital money emerged''?',
    'Metatheory: clarify the distinction between endorsing a reading and endorsing its truth. A reading is a coherent frame that makes certain facts visible and others invisible; a true claim would be one that is true regardless of frame. Is the ''became thinkable'' reading one way of organizing the same facts, or is it asserting a claim (temporal priority of conceptualization) that is either true or false across all frames?',
    'If the reading is a coherent frame, different frames (first_held, m4_m5_collapse) are alternative organizations of the same facts, and truth is not at stake—only utility and scope are. If the reading is asserting a temporal truth claim, then it is in genuine competition with the other readings, and one reading may be right and the others wrong. This affects how the engine computes resolution: frame-relativism leads to ''coexists_with''; truth-relativism leads to foreclosure possibilities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame__reading_distinction, preference, 'Epistemological status of kernel readings: frames or truth claims?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__became_thinkable_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(elec_tr_t10, electronic_money_emergence__became_thinkable_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(elec_tr_t20, electronic_money_emergence__became_thinkable_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(elec_tr_t30, electronic_money_emergence__became_thinkable_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(elec_tr_t40, electronic_money_emergence__became_thinkable_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(elec_tr_t50, electronic_money_emergence__became_thinkable_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(elec_be_t10, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(elec_be_t20, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(elec_be_t30, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(elec_be_t40, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(elec_be_t50, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 50, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t0, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(elec_su_t10, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement(elec_su_t20, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(elec_su_t30, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(elec_su_t40, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(elec_su_t50, electronic_money_emergence__became_thinkable_reading, suppression_requirement, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__became_thinkable_reading, 0.06).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The 'electronic_money_emergence' kernel has three structurally distinct readings instantiated as separate constraint stories. (1) This story: became_thinkable_reading — emergence dated to conceptualization. (2) Sibling: first_held_reading — emergence dated to first institutional bearer. (3) Sibling: m4_m5_collapse_reading — emergence is a measurement artifact retroactively imposed. The three readings share a referent (the historical moment and process of digital money's emergence) but assign emergence to different thresholds (concept / behavior / measurement). They form a constraint family linked via network.affects_constraints. See commentary.kernel_context for the structural relationship and omega variables documenting logical dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
