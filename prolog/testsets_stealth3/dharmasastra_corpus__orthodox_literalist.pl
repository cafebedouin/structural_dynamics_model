% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Orthodox Literalist Reading of the Dharmasastra Corpus
 *   domain: religious/normative/legal
 *
 * SUMMARY:
 *   The dharmasastra corpus — Manusmriti and the later smritis — prescribes a
 *   graded social order in which varna and jati position, assigned at birth,
 *   determines occupation, education, ritual access, legal weight, and
 *   marriage. This file instantiates ONE reading of that contested kernel:
 *   the orthodox literalist reading, under which the prescriptions are
 *   eternal revealed truth demanding literal observance. The ε referent is
 *   the standing arrangement under contest — the enforced literal-observance
 *   hierarchy itself — assessed by the analytical seat; the reading's own
 *   claims are carried separately in cs_structure.axioms and are not
 *   reconciled against the metrics. The claim/metric gap here is deliberate
 *   and small: I claim snare because the coordination story (maintenance of
 *   cosmic and social order) functions as cover for a status-extraction
 *   hierarchy whose persistence rides on coercion and exit-suppression, and I
 *   author metrics that independently describe that operation. Sibling
 *   readings (reformist_contextual, abolitionist_rejection) are separate
 *   constraint stories with their own ε and victim sets; they are linked, not
 *   folded into this file. KEY AGENTS (by structural relationship): -
 *   brahmin_ritual_elites: agenda-setter and principal beneficiary
 *   (institutional/arbitrage) — interprets, enforces, collects -
 *   twice_born_upper_castes: beneficiary with enforcement backing
 *   (powerful/constrained) - dalit_outcaste_laborers: primary target, zero
 *   benefit channel (powerless/trapped) - shudra_service_castes: primary
 *   target with residual economic leverage (moderate/constrained) -
 *   women_of_all_varnas: target at every class level
 *   (powerless/identity_locked) - anti_caste_reform_movements: excluded voice
 *   with mass following (organized/mobile) - constitutional_state: analytical
 *   observer prosecuting enforcement (institutional/analytical)
 *
 * KEY AGENTS:
 *   - brahmin_ritual_elites: agenda_setter + beneficiary (institutional power, arbitrage exit, continental scope) — controls interpretation and collects the fee, endowment, and honor flows
 *   - twice_born_upper_castes: beneficiary + agenda_setter (powerful, constrained, continental) — receives legitimation and favorable labor order, supplies coercive backing
 *   - dalit_outcaste_laborers: payer (powerless, trapped, continental) — bears the fullest extraction with no benefit channel and no affordable exit
 *   - shudra_service_castes: payer (moderate, constrained, continental) — pays dues and deference; wealth cannot convert to ritual standing
 *   - women_of_all_varnas: payer (powerless, identity_locked, continental) — regulated by the same codes at every class level; identity fused with the duty structure
 *   - anti_caste_reform_movements: excluded (organized, mobile, continental) — objects from outside the orthodox adjudicating forum
 *   - constitutional_state: observer (institutional, analytical, national) — records, prosecutes, and measures from outside the frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.9).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.85).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Orthodox Literalist Reading of the Dharmasastra Corpus").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious/normative/legal").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe').
narrative_ontology:cs_kernel_codification('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', fixed_text).
narrative_ontology:cs_authority_grounding('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', lineage).
narrative_ontology:cs_interpretation_layer_present('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe').
narrative_ontology:cs_reading_relation('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', foundational, smriti_injunctions_eternal_and_literally_binding).
narrative_ontology:cs_axiom_status(smriti_injunctions_eternal_and_literally_binding, holdable).
narrative_ontology:cs_axiom_grounding('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', smriti_injunctions_eternal_and_literally_binding, theological).
narrative_ontology:cs_axiom('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', foundational, birth_ascribed_varna_duty_is_dharma_itself).
narrative_ontology:cs_axiom_status(birth_ascribed_varna_duty_is_dharma_itself, holdable).
narrative_ontology:cs_axiom_grounding('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', birth_ascribed_varna_duty_is_dharma_itself, theological).
narrative_ontology:cs_reference_frame('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', eternal_revealed_varna_order).
narrative_ontology:cs_drift_state('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', constitutional_repudiation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1fe08e9c-16c2-4ed9-8f56-7fa1de5c1abe', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_elites).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, twice_born_upper_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalit_outcaste_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_service_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_of_all_varnas).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, karma_samsara_theodicy).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_ashrama_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, apaurusheya_revelation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret, transmit, and adjudicate the corpus: pandit lineages recite and gloss the smritis, certify ritual validity, staff temples and advise royal courts, and decide what counts as authoritative observance. They receive dakshina (ritual fees), dedicated land endowments, and first claim on honor and teaching authority, and reproduce themselves through lineage-controlled education. Their physical repositioning under new regimes has historically been easy, but their status exists only while the corpus remains authoritative, so they defend its literality.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_elites, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_elites, beneficiary).

% Rulers, warriors, merchants, and landholders who receive legitimation of rule and wealth, priority in ritual and schooling, and a labor order structured to their advantage. They fund the priestly apparatus and supply the coercive backing that enforces varna duties. Their own obligations (protection, generosity, ritual propriety) are real but cheap relative to what the order returns to them; their standing is defined relative to the hierarchy, so abandoning the frame would cost them their own rank.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, twice_born_upper_castes, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, twice_born_upper_castes, agenda_setter).

% Perform the labor the purity system requires but refuses to acknowledge: scavenging, leatherwork, corpse handling, landless field labor. Barred from Vedic education, temple entry, wells, and village commons; subject to unpunished violence and hereditary bondage. The karma frame marks their condition as deserved, so the tradition offers no doctrinal exit; conversion historically followed them socially. Flight to frontier districts meant losing every tie at ruinous cost.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_outcaste_laborers, payer,
    powerless, generational, trapped, continental).

% Farmers, artisans, and village servants who sustain the agrarian and craft economy, paying dues to temples and priests and owing deference and service upward. Denied Vedic education and ritual office, their literacy ran through vernacular and practical channels; some jatis accumulated wealth and local clout that could never convert into ritual standing. Exit existed in migration, new jati formation, or heterodox affiliation, but always at the price of protection networks and marriage ties.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_service_castes, payer,
    moderate, generational, constrained, continental).

% Regulated by the same textual apparatus across every varna: pativrata ideals, child-marriage norms, exclusion from Vedic learning and independent property in classical prescription, sati glorification at the extreme. Upper-caste women draw status from the hierarchy while bearing its tightest restrictions; lower-caste women work and face sexual vulnerability the same codes excuse. Their identity is formed inside the framework from childhood — wife-and-mother duty taught as their own dharma — so psychological exit is closed even where physical escape is conceivable.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_of_all_varnas, payer,
    powerless, biographical, identity_locked, continental).

% Buddhist sanghas historically, low-caste Bhakti teachers, Phule's Satyashodhak Samaj, and Ambedkar's movement reject the corpus's authority in whole or part, build separate schools, wells, and congregations, and demand representation. The orthodox forum does not recognize their testimony — pandits do not adjudicate with them — yet they command mass followings and eventually constitutional leverage.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, anti_caste_reform_movements, excluded,
    organized, generational, mobile, continental).

% The post-independence legal order abolishes untouchability outright, criminalizes enforcement of caste disabilities, and runs reservation policy. It observes and records the regime's operation, prosecutes enforcement violence, and measures outcomes; it adjudicates in its own frame and does not deliberate inside the orthodox one.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, constitutional_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_elites).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large agrarian society's division of labor, ritual precedence, marriage regulation, and dispute adjudication through ascribed hereditary stations, and supplies a single normative code covering law, ritual, diet, and conduct across regions and generations.
% TRANSFER_FUNCTION: Moves agricultural surplus, hereditary service labor, ritual fees and offerings, first claim on education and public honor, and deference itself from Shudra and Dalit workers and from all women to Brahmin ritual specialists and upper-caste households; moves pollution-bearing labor downward onto those forbidden to refuse it.
% ABSENT_VOICES: Those the system renders voiceless inside it: Dalits, whose testimony the corpus itself discounts and whose presence is held to pollute the assembly; Shudras barred from the learning that would license speaking; women whose ritual speech is prescribed rather than free. They object from outside the orthodox forum — Buddhist sanghas historically, Satyashodhak and Ambedkarite movements in the modern era — and their eventual constitutional leverage is why the reading now fights a repudiation front.
% DISAPPEARANCE_RATIONALE: Marriage markets, village labor allocation, temple economies, legal procedure, and the status architecture of a subcontinent were organized around the hierarchy; overnight loss forces immediate renegotiation of who may marry, work, teach, enter where, and sit beside whom — violently in places, transform everywhere.
% FOUNDING_PROBLEM: After the Vedic period a large plural society with competing cults, rising merchant wealth, and new dynasties needed one normative order: who may teach, sacrifice, rule, marry, inherit, and eat with whom. The smritis codified answers and embedded a graded hierarchy as the ordering spine.
% FOUNDING_PROBLEM_CORROBORATION: Indological and legal-historical scholarship outside the tradition attests the corpus originated as an elite codification answering a specific consolidation moment, not a universal human need; modern constitutional jurisprudence attests the ordering function is now performed otherwise. Only the orthodox beneficiary set attests the founding problem as still live — and that attestation coming solely from beneficiaries is itself signal.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.9, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.90: extraction spans material transfers (hereditary service obligations, temple dues), blocked capability formation (Vedic education barred to Shudras and all women), legal disability (graded penalties and property limits in classical prescription), and dignity stripping (untouchability). Suppression 0.85 is raw structural force, authored unscaled per the framework rule — the engine scales only extractiveness, by directionality and continental scope: purity/pollution sanctions, social death for transgressors, hereditary ascription closing birth exit, and violence against boundary-crossing. Theater_ratio 0.32 is deliberately moderate-low: much of the ritual apparatus is functionally load-bearing FOR the hierarchy (purity observance polices boundaries; ceremony stages rank), so it is instrumental rather than decorative; only a minority is pure performance. Accessibility_collapse 0.65: alternatives existed and persist (heterodox orders, conversion, migration, constitutional citizenship) but collapse under heavy cost — karma theodicy prices rebellion as self-harm across lifetimes. Resistance 0.75: an unbroken counter-tradition from Buddhist sanghas through Bhakti to Phule and Ambedkar. Identity lock closes exit ideologically (station as earned dessert) and relationally (pativrata duty constituted as women's own dharma); where the ideological frame broke — mass conversion events — suppression demand dropped visibly, which is the internalization tell routed to the omega below. The three series share one nine-point grid; the mid-series dip models heterodox-challenge phases (Bhakti-era softening) before an enforcement ratchet (colonial courts and census codification hardened textual authority) carried extraction and suppression to their pre-repudiation maxima at interval end.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes divergent per-seat types from the structural data. From the brahmin_ritual_elites seat the arrangement presents as stewarded sacred order — subsidy-side, defended as civilization itself. From twice_born_upper_castes it presents as legitimate reciprocity: their protective and generous obligations are real, but cheap relative to returned rents. From dalit_outcaste_laborers the same structure is total extraction with no benefit channel whatsoever. From women_of_all_varnas it is extraction layered over a genuine status return for upper-caste members — a dual position a single power atom cannot separate, which is why no directionality override is authored and the ambiguity is routed to the omega variables instead.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward the subsidy pole: brahmin_ritual_elites collect fees, endowments, and first honors while controlling adjudication; twice_born_upper_castes receive legitimation, priority, and a favorable labor order. Victim declarations drive d toward the full-target pole: dalit_outcaste_laborers sit nearest 1.0 — maximal extraction, zero benefit, no exit; shudra_service_castes sit near-full target with slight damping from residual economic leverage; women_of_all_varnas sit near-full target with damping from the status returns upper-caste women draw from the same hierarchy. Continental spatial scope amplifies effective extraction for targets — verifying abuse across ten thousand villages is cheap for the enforcement core and ruinously expensive for isolated victims — and the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem: consolidating a post-Vedic plural society under one normative order — solved long ago by states, codified civil law, markets, and constitutional citizenship; status: dead. Disappearance verdict: world_rearranges, because millions of lives were arranged by it. Dead-status crossed with rearranges-verdict is the zombie signature: the arrangement persists without its mandate, sustained by inertia, concentrated beneficiary defense, and internalized acceptance. The snare classification prevents the still-visible jati-coordination surface (mutual aid, occupational transmission, marriage networks) from laundering the enforcement core as coordination; a piton reading fails because beneficiaries remain concentrated enough to actively maintain enforcement — the administrator could change the arrangement, but fixing it dissolves the administrator's own position, which is why fixing_cost is authored prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the orthodox_literalist reading of the dharmasastra_corpus kernel — what would the sibling readings change structurally?',
    'Compare classifications across the linked family files (dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection): victim sets, epsilon, and enforcement requirements differ per reading while the referent (the standing enforced-hierarchy arrangement) stays fixed.',
    'Under reformist_contextual the victim set shrinks to those harmed by the retained caste prescriptions and epsilon falls materially; under abolitionist_rejection the authority structure itself becomes the extraction object. This file''s classification is pinned to this reading and should not shift under either sibling''s resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the dharmasastra corpus kernel; sibling deltas recorded, contest located in the eternity/literality axiom.').

omega_variable(
    residual_coordination_function,
    'Does the regime retain genuine coordination value — jati-level mutual aid, occupational transmission, marriage regulation — sufficient to make it tangled_rope rather than snare?',
    'Decompose jati-welfare functions from hierarchy-enforcement functions and measure extraction net of coordination value; use natural experiments from communities that kept jati networks while shedding purity enforcement (urban diaspora, post-conversion communities).',
    'If coordination value proves substantial and separable from the hierarchy, reclassification toward tangled_rope is warranted; this authoring judges the coordination story to operate largely as cover for the enforced hierarchy, with the separability claim itself belonging to the rival reformist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coordination_function, empirical, 'Whether surviving coordination function upgrades the constraint from snare to tangled_rope.').

omega_variable(
    suppression_internalization_share,
    'Is the measured suppression structural (economic dependency, violence, legal disability) or internalized (karma-theodicy acceptance, pativrata identity fusion)?',
    'Post-exit suppression trajectory: track deference patterns and purity anxiety in communities after conversion, migration, or legal emancipation — persistence after external barriers fall indicates the internalized share.',
    'If internalization carries a large share, effective suppression exceeds the structural measure and outlasts formal abolition — as observed after constitutional repudiation — shifting suppression-dependent seat computations without changing the constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_share, empirical, 'Structural versus internalized suppression mechanism in caste and gender discipline.').

omega_variable(
    text_vs_practice_causality,
    'How much of the observed extraction is caused by the textual prescriptions versus merely ratified by them — would the hierarchy persist substantially unchanged if textual authority vanished overnight?',
    'Comparative analysis of caste-like hierarchies in societies without dharmasastric textualization, and of enforcement variance across regions operating under identical texts.',
    'If social practice drives the text rather than vice versa, the effective removal target is the enforcement apparatus rather than the corpus, and epsilon attribution between text and practice shifts accordingly; the orthodox reading''s own claim that the texts originate the order would lose its warrant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_vs_practice_causality, conceptual, 'Attribution of extraction between textual authority and underlying social practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__orthodox_literalist, theater_ratio, 10, 0.22).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__orthodox_literalist, theater_ratio, 20, 0.24).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__orthodox_literalist, theater_ratio, 30, 0.26).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.28).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__orthodox_literalist, theater_ratio, 50, 0.3).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__orthodox_literalist, theater_ratio, 60, 0.31).
narrative_ontology:measurement(dhar_tr_t70, dharmasastra_corpus__orthodox_literalist, theater_ratio, 70, 0.32).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__orthodox_literalist, theater_ratio, 80, 0.32).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(dhar_be_t70, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 70, 0.85).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 80, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 50, 0.64).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(dhar_su_t70, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 70, 0.8).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 80, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the dharmasastra_corpus kernel per the ε-invariance principle: one text-corpus label covers three structurally distinct constraints. This file carries the orthodox_literalist reading (eternal literal observance; expansive victim set; enforcement concentrated in the interpreting lineage). Upstream textual authority feeds downstream enforcement intensity in both sibling files; reformist_contextual shrinks the victim set and lowers ε by detaching the ethical core from the hierarchy, while abolitionist_rejection relocates the extraction object to the authority structure itself. Cross-links propagate contamination analysis across the family; no reading averages over another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
