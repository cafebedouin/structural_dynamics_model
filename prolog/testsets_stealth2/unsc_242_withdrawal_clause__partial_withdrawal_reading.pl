% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Partial-Withdrawal Reading: Discretionary Withdrawal Scope with Secure-Boundary Retention
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   A single colloquial label — the withdrawal clause of UN Security Council
 *   Resolution 242 — covers three structurally distinct commitments,
 *   decomposed per the epsilon-invariance principle into a linked family.
 *   This file instantiates the partial_withdrawal_reading: the operative
 *   arrangement under which the scope of required withdrawal is
 *   discretionary, grounded in the drafters' choice of the indefinite English
 *   article ('withdrawal ... from territories occupied'), with the call for
 *   'secure and recognized boundaries' read as licensing retention of
 *   strategically valued territory pending negotiation. Under this reading
 *   the clause functions as a ledger that converts textual indefiniteness
 *   into sequenced negotiating leverage: the occupying power decides in
 *   practice what counts as secure boundaries, mediating powers define the
 *   phases and certify reciprocity, and claimant states receive territory
 *   only as offered terms are accepted. The epsilon referent is this standing
 *   discretionary arrangement, assessed by this reading's own lights: the
 *   reading accepts the framework's legitimacy as the price of any agreement
 *   at all, while recording that its conditionality runs without a fixed
 *   enforcement line for the claimant side. Sibling files author different
 *   epsilons over different referents: the maximal_withdrawal_reading
 *   measures the retention arrangement against a mandatory full-withdrawal
 *   demand (high epsilon), and the interpretive_authority_structure measures
 *   the adjudication arrangement itself. KEY AGENTS (by structural
 *   relationship): - occupying_power: Primary beneficiary and de facto
 *   line-setter (institutional/arbitrage) — retains strategic territory and
 *   controls what 'secure boundaries' means in practice - mediating_powers:
 *   Agenda-setter (institutional/mobile) — broker phases, define reciprocity,
 *   collect process-control rents - security_council_permanent_members:
 *   Secondary beneficiaries (institutional/arbitrage) — preserve interpretive
 *   discretion and reusable precedent through selective enforcement -
 *   territorial_claimant_states: Primary target among states
 *   (moderate/constrained) — receive territory only on offered terms, with no
 *   fixed enforcement line - occupied_population: Primary human target
 *   (powerless/trapped) — lives the deferred final status under military
 *   administration - general_assembly_majority: Excluded voice
 *   (organized/constrained) — records the maximal position without an
 *   enforcement seat - international_court_of_justice: Analytical observer
 *   (institutional/analytical) — issues opinions that enter the record
 *   without compelling anyone
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.54).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Partial-Withdrawal Reading: Discretionary Withdrawal Scope with Secure-Boundary Retention").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'c83fe925-81a5-4cdf-a2b8-01b987ae12b9').
narrative_ontology:cs_kernel_codification('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', fixed_text).
narrative_ontology:cs_authority_grounding('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', lineage).
narrative_ontology:cs_interpretation_layer_present('c83fe925-81a5-4cdf-a2b8-01b987ae12b9').
narrative_ontology:cs_reading_relation('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', unsc_242_withdrawal_clause__maximal_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', foundational, withdrawal_scope_drafters_discretionary).
narrative_ontology:cs_axiom_status(withdrawal_scope_drafters_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', withdrawal_scope_drafters_discretionary, conventional).
narrative_ontology:cs_axiom('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', foundational, secure_boundaries_permit_territorial_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_permit_territorial_retention, holdable).
narrative_ontology:cs_axiom_grounding('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', secure_boundaries_permit_territorial_retention, instrumental).
narrative_ontology:cs_reference_frame('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', drafters_intent_phased_withdrawal_framework).
narrative_ontology:cs_drift_state('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c83fe925-81a5-4cdf-a2b8-01b987ae12b9', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_powers).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, security_council_permanent_members).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimant_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupied_population).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafters_intent_travaux_canon).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, indefinite_article_discretionary_scope).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, secure_recognized_boundaries_principle).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, land_for_peace_reciprocity_formula).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds territory taken in the June 1967 war and decides in practice which portions fall inside the 'secure and recognized boundaries' it will accept. Signs onto the resolution's framework while building settlements and positioning forces in the areas it intends to retain. Invokes the text's flexibility when retaining and its withdrawal language when trading; abandoning the framework entirely would cost it the recognition and normalization channels the framework opens.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter).

% Broker the sequence: shuttle diplomacy, framework documents, guarantee regimes, conference chairs. Define what counts as a phase, what reciprocity requires, and when conditions are met. The open-ended scope of withdrawal keeps them indispensable as interpreters and guarantors; they can step back from mediation at diplomatic cost but cannot be forced out of the role.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_powers, agenda_setter,
    institutional, generational, mobile, global).

% Adopted the resolution and oversee its implementation selectively. The veto lets each member protect preferred interpretations; the resolution's flexibility supplies a reusable template for managing other territorial disputes without binding precedent. Their gains are indirect: preserved discretion, precedent value, and alliance-management room.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, security_council_permanent_members, beneficiary,
    institutional, generational, arbitrage, global).

% Assert sovereignty over the territories and accepted the resolution expecting withdrawal. Receive territory only through negotiated phases whose pace and content the stronger party and the mediators control. Military recovery failed in 1973; adjudicatory routes yield opinions without enforcement; the veto blocks binding council action. Their remaining options are bilateral negotiation on offered terms or indefinite waiting.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimant_states, payer,
    moderate, generational, constrained, regional).

% Live under military administration in the territories whose final status the framework defers. Movement, residency, and resources are administered by the occupying power while negotiations proceed without a seat for them at the table. Physical exit means displacement; legal exit routes run through institutions that issue opinions nobody is compelled to execute.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupied_population, payer,
    powerless, biographical, trapped, local).

% Passes recurring resolutions affirming the inadmissibility of acquiring territory by war and calling for withdrawal from all the territories. Records its position annually but holds no enforcement seat; its texts are cited by claimants and courts yet change nothing on the ground without council concurrence.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, general_assembly_majority, excluded,
    organized, generational, constrained, global).

% Responds to requests for advisory opinions on the legality of structures and policies in the territories; its 2004 wall opinion restated the prohibition on acquiring territory by force. It cannot compel any party; its interpretations enter the record and are taken up or set aside by the political organs.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a fixed, mutually exclusive pair of demands (full withdrawal versus no withdrawal without peace) into a sequenced exchange both sides could enter: territory moves in phases, recognition and normal relations move in phases, and neither side must pre-commit to final lines before starting.
% TRANSFER_FUNCTION: Moves discretion over territorial scope and sequencing from the claimant states and the occupied population to the occupying power and the mediating powers; moves diplomatic roles, guarantee functions, and process-control rents to the mediators; moves recognition and normalization toward the occupying power as phases complete.
% ABSENT_VOICES: The occupied population had no seat at the drafting table in 1967 and none at the mediation tables where 'secure and recognized boundaries' acquired operational content; the General Assembly majority records objections it cannot enforce; claimant states negotiate only bilaterally, on terms the stronger party frames.
% DISAPPEARANCE_RATIONALE: If the discretionary reading lost operative force overnight, the operative baseline would become the full-withdrawal demand: settlement legitimacy would collapse, the phased frameworks built on reciprocal conditionality (disengagement agreements, the Camp David architecture, the Oslo interim regime) would lose their interpretive foundation, and the mediation apparatus organized around defining phases would dissolve or reconstitute around a fixed line.
% FOUNDING_PROBLEM: After the June 1967 war: consolidate the ceasefires, obtain Israeli withdrawal, and obtain Arab acceptance of Israel's existence, when each side demanded the other move first and no side would sign text committing it unconditionally.
% FOUNDING_PROBLEM_CORROBORATION: Claimant-state diplomatic correspondence and General Assembly recitals attest that the withdrawal half of the founding problem remains unresolved; the ICJ's 2004 advisory proceedings and independent drafting-history scholarship corroborate that the clause was deliberately crafted ambiguous to permit signature, disputing only whether that ambiguity was an expedient bridge or a standing license. The occupying power and the mediating powers attest the problem is being worked progressively — attestation splits along beneficiary lines, with corroboration of the problem's persistence coming from outside the beneficiary set.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 at interval end: extraction is real but conditional and phased — territory moved under this structure (the Sinai return, the disengagement accords), yet discretion over scope and sequence accrues to the side holding the ground, and no phase auto-executes against non-performance. Suppression 0.54 is authored as a raw structural property, unscaled by power or scope: the arrangement persists because the maximal reading is actively kept off the books — veto deployment over enforcement attempts, steering claimants into bilateral tracks, guarantee regimes that bind the weaker side to the sequence while nothing binds the stronger side to a terminus. Theater 0.50: roughly half of observable activity (reaffirmations, conferences, envoys, interim administrations) performs process without moving lines, while the core function still executes when phases close. Accessibility_collapse 0.38: alternatives never fully close — the maximal reading stays arguable, advisory-opinion routes stay open, the General Assembly re-records its position annually — which is precisely why the ambiguity remains usable. Resistance 0.62: sustained claimant-state refusal, assembly majorities, court requests, and periodic uprising cycles. Temporal data run on one shared eight-point grid (1967-2024) with all three tracked metrics authored at every point; the suppression_requirement series is authored deliberately because the story traces an enforcement-capacity arc — build-up through the guarantee and veto machinery of 1973-2004, slight erosion afterward as enforcement credibility costs rose. Coalition note: the occupied population's coalition potential (assembly majorities, boycott movements) is real but does not convert into enforcement, which is why its seat computes at the trapped end despite mass participation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats compute differently from identical text. From the mediating and occupying seats the clause is a working exchange protocol they operate: phases execute, territory moves, recognition flows, and the indefiniteness is what made signature possible in 1967 at all. From the claimant-state seat the same protocol is a sequence whose pace and content are set by the stronger party, with no fixed line at which their entitlement crystallizes into something enforceable; from the occupied-population seat it is an indefinitely deferred final status administered daily by the other side. The engine derives these divergent per-seat classifications from the structural data (exit options, power, declared roles); the divergence is the finding, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure maps to directionality as follows. occupying_power sits nearest the beneficiary pole: it collects retention directly, holds arbitrage-grade interpretive mobility, and pays little under an arrangement it administers in practice. mediating_powers derive low d from their beneficiary declaration, but their gain is contingent on process continuation and they bear real diplomatic costs; the derivation slightly understates their d — however, the override surface keys on power atoms alone and would misfire across the four institutional seats (occupying power, mediators, permanent members, court), so no override is authored and the imprecision is recorded here instead. security_council_permanent_members hold diffuse, indirect benefit (preserved discretion, reusable precedent) — a modest d above the pure-beneficiary floor. territorial_claimant_states sit near the target pole: constrained exit (the military option closed in 1973, adjudicatory routes unenforceable, the veto blocking binding action) pushes their effective extraction toward the full-target end. occupied_population sits at the extreme target end: trapped, powerless, bearing the arrangement's daily costs with no exit that is not displacement. The constraint's regional scope, with bilaterally opaque verification of what 'secure boundaries' contains, lets the engine scale effective extraction upward for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — consolidating the 1967 ceasefires and getting each side to move first without unconditional commitment — is substantially spent: ceasefires held, two claimant states made peace, and the emergency that justified elastic language has receded into history. Yet the arrangement persists and its extraction accumulates (settlement expansion raises the price of every future phase), which is the classic mandate-outlived-function profile. The contested founding_problem_status paired with a world_rearranges disappearance verdict flags the zombie/capture dynamic for the mismatch consumer. The tangled_rope classification prevents two opposite errors: a pure-coordination reading would ignore that the coordination output (executed phases) is gated by the very discretion the structure creates for the stronger party; a pure-extraction reading would ignore that real territory changed hands under exactly this text. The theater trajectory (0.18 to 0.50) tracks the drift toward performance; if theater decisively exceeded the functional share with no executed phases, the residue would look inertial — but a concentrated beneficiary still profits from the arrangement, which keeps the endpoint snare-flavored rather than piton-shaped.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the unsc_242_withdrawal_clause kernel; what would the sibling readings change structurally?',
    'Compare the compiled stories across the kernel family: the maximal_withdrawal_reading shifts the victim set to include all retained territories and removes mediator discretion; the interpretive_authority_structure relocates the contest from substance to adjudicative competence.',
    'If the maximal reading displaced this one, the epsilon referent becomes the full-withdrawal demand and the occupying power flips from beneficiary to target; if the authority-structure reading dominates, classification migrates to the meta-level contest over who may resolve the text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: this story is the partial_withdrawal_reading of kernel unsc_242_withdrawal_clause, with two sibling readings held as separate constraints.').

omega_variable(
    authentic_text_article_control,
    'Which authentic text controls the clause''s scope — the English indefinite article (''from territories'') or the French definite article (''des territoires'')?',
    'Vienna Convention Article 33 comparative-authentic-text analysis plus full drafting-travaux disclosure; examination of how international tribunals treat equally authentic multilingual instruments.',
    'French-text control dissolves this reading''s textual foundation and merges it into the maximal reading; English-text control entrenches the discretion this story models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_text_article_control, empirical, 'Authentic-text selection determines whether the discretionary scope is textually grounded at all.').

omega_variable(
    constructed_vs_neutral_interpretation,
    'Is the discretionary regime a neutral interpretive necessity, or a constructed arrangement whose indefiniteness was deliberately engineered to benefit identifiable agents?',
    'Drafting-history record: compare stated drafter objectives against distributional outcomes across the interval; test whether precise alternative formulations were proposed during drafting and rejected, and by whom.',
    'Deliberate engineering supports reclassification pressure toward the extraction-dominated end of the hybrid range; neutral necessity supports the coordination-weighted reading of the same structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_neutral_interpretation, empirical, 'Whether the indefiniteness was engineered negotiating leverage or an unavoidable drafting compromise.').

omega_variable(
    enforcement_line_existence,
    'Does the phased framework contain any terminus at which the claimants'' entitlement crystallizes into an enforceable line, or is the conditionality open-ended by design?',
    'Textual analysis of the subsequent-framework clauses (Camp David, the Oslo interim periods) plus a behavioral test: has any phase ever auto-triggered consequences upon non-performance?',
    'Open-ended conditionality confirms the ledger structure (indefiniteness converted into leverage) and raises effective extraction for the payer seats; a real terminus would damp effective extraction and support the coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_line_existence, empirical, 'Whether the conditionality ever terminates in an enforceable obligation or runs indefinitely.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (post-June-1967 stabilization and reciprocal de-escalation) dead, such that the arrangement now persists mainly as a negotiating instrument?',
    'Cross-check the founding_problem_status against disappearance behavior: if removing the discretionary reading would not destabilize any live ceasefire or treaty architecture, the original mandate is obsolete.',
    'A dead mandate combined with world_rearranges persistence flags capture/zombie dynamics and pushes the classification toward the extraction-dominated end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Mandatrophy exposure of the partial-withdrawal framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(u242_partial_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(u242_partial_tr_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(u242_partial_tr_t1979, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1979, 0.28).
narrative_ontology:measurement(u242_partial_tr_t1988, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1988, 0.34).
narrative_ontology:measurement(u242_partial_tr_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(u242_partial_tr_t2004, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2004, 0.45).
narrative_ontology:measurement(u242_partial_tr_t2017, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2017, 0.48).
narrative_ontology:measurement(u242_partial_tr_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2024, 0.5).

% Extraction over time
narrative_ontology:measurement(u242_partial_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(u242_partial_be_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement(u242_partial_be_t1979, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1979, 0.5).
narrative_ontology:measurement(u242_partial_be_t1988, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1988, 0.53).
narrative_ontology:measurement(u242_partial_be_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1993, 0.56).
narrative_ontology:measurement(u242_partial_be_t2004, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2004, 0.59).
narrative_ontology:measurement(u242_partial_be_t2017, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2017, 0.61).
narrative_ontology:measurement(u242_partial_be_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(u242_partial_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.28).
narrative_ontology:measurement(u242_partial_su_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1973, 0.36).
narrative_ontology:measurement(u242_partial_su_t1979, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1979, 0.46).
narrative_ontology:measurement(u242_partial_su_t1988, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1988, 0.5).
narrative_ontology:measurement(u242_partial_su_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1993, 0.54).
narrative_ontology:measurement(u242_partial_su_t2004, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2004, 0.58).
narrative_ontology:measurement(u242_partial_su_t2017, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2017, 0.56).
narrative_ontology:measurement(u242_partial_su_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2024, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, resource_allocation).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Resolution 242 withdrawal clause'. The label conflates three structurally distinct constraints: the substantive-maximal claim (withdrawal mandatory from all territories; French definite article controls; Charter 2(4) default), the substantive-partial claim instantiated here (scope discretionary per drafters' intent; secure-boundaries retention licensed), and the meta-level authority claim (who may resolve the textual ambiguity — court, drafting states, or occupying-state practice). Their epsilon values differ because their referents differ: the maximal reading measures the retention arrangement against a mandatory-demand baseline; this reading measures the discretionary arrangement by its own lights (conditional, phased, moderate); the authority-structure reading measures the adjudication arrangement itself. Edges run upstream-downstream: whichever reading wins the authority contest determines which substantive reading binds, and this reading's accumulated practice creates structural pressure on the authority contest without resolving it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
