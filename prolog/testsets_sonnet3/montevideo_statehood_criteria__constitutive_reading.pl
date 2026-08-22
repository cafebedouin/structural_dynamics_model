% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Requirement for Statehood
 *   domain: international_law/political
 *
 * SUMMARY:
 *   This story instantiates the CONSTITUTIVE reading of the Montevideo
 *   statehood kernel: statehood is not merely a description of a factual
 *   condition but a legal status that comes into being only when the existing
 *   community of states extends recognition. Under this reading, an entity
 *   satisfying territory, population, government, and capacity for external
 *   relations is not yet a state in the full legal sense until enough of the
 *   existing states say so. This produces a structural veto held by
 *   incumbent, already-recognized states (especially the UN Security Council
 *   permanent members and regional hegemons) over the entry of new
 *   competitors into the community of sovereigns. The sibling readings —
 *   declaratory (objective criteria alone suffice) and hybrid (objective
 *   criteria plus normative legitimacy) — are NOT part of this constraint;
 *   they are separate constraints with their own ε and their own victim sets,
 *   linked here only via the kernel-contest structure recorded in
 *   cs_structure and the omega variables below.
 *
 * KEY AGENTS:
 *   - permanent_security_council_members: agenda_setter (institutional/arbitrage) — control UN admission and hold effective veto over recognition consequences
 *   - regional_hegemons: agenda_setter/beneficiary (powerful/arbitrage) — coordinate regional recognition to preserve favorable territorial arrangements
 *   - existing_recognized_states: beneficiary (organized/mobile) — insulated incumbents whose own status is never re-litigated
 *   - unrecognized_polities: payer (powerless/trapped) — meet objective criteria on the ground but lack treaty capacity, IFI access, or diplomatic immunity
 *   - de_facto_states: payer (powerless/trapped) — long-functioning governments excluded by patron-rivalry dynamics rather than governance failure
 *   - aspirant_secessionist_populations: payer (powerless/trapped) — local mandate insufficient without outside assent
 *   - patron_states: beneficiary/agenda_setter (powerful/arbitrage) — use selective recognition as a foreign-policy lever at low cost to themselves
 *   - international_law_scholars: observer (analytical) — document the constitutive/declaratory divide without altering practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.71).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.78).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Requirement for Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '08d59a67-2b76-4cd0-a061-1a99422b2f6b').
narrative_ontology:cs_kernel_codification('08d59a67-2b76-4cd0-a061-1a99422b2f6b', distributed).
narrative_ontology:cs_authority_grounding('08d59a67-2b76-4cd0-a061-1a99422b2f6b', practice).
narrative_ontology:cs_interpretation_layer_present('08d59a67-2b76-4cd0-a061-1a99422b2f6b').
narrative_ontology:cs_reading_relation('08d59a67-2b76-4cd0-a061-1a99422b2f6b', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('08d59a67-2b76-4cd0-a061-1a99422b2f6b', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('08d59a67-2b76-4cd0-a061-1a99422b2f6b', foundational, recognition_is_legally_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(recognition_is_legally_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('08d59a67-2b76-4cd0-a061-1a99422b2f6b', recognition_is_legally_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('08d59a67-2b76-4cd0-a061-1a99422b2f6b', secondary, existing_states_hold_legitimate_discretion_over_new_entrants).
narrative_ontology:cs_axiom_status(existing_states_hold_legitimate_discretion_over_new_entrants, holdable).
narrative_ontology:cs_axiom_grounding('08d59a67-2b76-4cd0-a061-1a99422b2f6b', existing_states_hold_legitimate_discretion_over_new_entrants, conventional).
narrative_ontology:cs_reference_frame('08d59a67-2b76-4cd0-a061-1a99422b2f6b', post_westphalian_incumbent_consent_framework).
narrative_ontology:cs_drift_state('08d59a67-2b76-4cd0-a061-1a99422b2f6b', post_1945_objective_criteria_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08d59a67-2b76-4cd0-a061-1a99422b2f6b', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, regional_hegemons).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_recognized_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, de_facto_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, aspirant_secessionist_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, patron_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit atop the recognition machinery: their bilateral recognition decisions, and their control of UN admission via Security Council recommendation, function as the practical gate through which a polity becomes a treaty partner, an IMF/World Bank member, and a holder of sovereign immunity. They can withhold recognition indefinitely without needing to justify the withholding on the objective-criteria merits, and they use this leverage to extract policy concessions from aspirant polities.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, permanent_security_council_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Coordinate recognition or non-recognition of breakaway or aspirant entities within their sphere to preserve territorial arrangements favorable to themselves (buffer states, resource corridors, alliance structures). Their recognition or refusal materially determines whether a claimant polity can access regional institutions.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, regional_hegemons, agenda_setter,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, regional_hegemons, beneficiary).

% Enjoy the presumption of continuity: once recognized, a state's territorial and juridical standing is not re-litigated each time a new claimant appears. They benefit from a system in which the bar for admitting competitors to statehood is high and controlled by the incumbents themselves, insulating existing borders and UN seat allocations from constant contestation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_recognized_states, beneficiary,
    organized, generational, mobile, global).

% Control territory, exercise effective government, and interact with a permanent population, but cannot sign treaties in their own name, cannot access the IMF or World Bank, cannot open embassies, and cannot invoke state immunity abroad. Their populations bear the practical cost — no consular protection, no international legal personality — regardless of how fully they satisfy the objective Montevideo criteria on the ground.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, biographical, trapped, national).

% Have functioned for years or decades with government, territory, population, and capacity for external relations, yet remain excluded from formal international personality because no critical mass of existing states will extend recognition, often for reasons tied to the interests of a patron state's rivals rather than to the entity's own governance record.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, de_facto_states, payer,
    powerless, generational, trapped, national).

% Live under a government they did not choose to remain under, pursuing independence through referenda or declarations, but find that even a clean local mandate cannot convert into statehood without the assent of the parent state's allies and the broader recognition-granting community; their legal status remains suspended on the decisions of outside capitals.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, aspirant_secessionist_populations, payer,
    powerless, biographical, trapped, national).

% Extend selective recognition to client or allied breakaway entities as a foreign-policy instrument, gaining strategic leverage over the parent state while bearing little cost themselves, since the constitutive framework lets them convert political preference directly into legal consequence for the claimant polity.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, patron_states, beneficiary,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, patron_states, agenda_setter).

% Document and debate whether recognition is constitutive or merely declaratory of a pre-existing legal fact, drawing on state practice, ICJ opinions, and admission patterns; their scholarship shapes doctrine but does not itself alter which polities are actually treated as states by the powerful.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the existing states with a shared, predictable gatekeeping mechanism for deciding which new entities enter the treaty system, the UN, and international financial institutions — avoiding a chaotic scramble where every state individually and inconsistently decides personality questions with no common reference point.
% TRANSFER_FUNCTION: Moves practical sovereignty — treaty capacity, financial-institution access, diplomatic immunity, consular protection — from polities that meet the objective governance criteria on the ground to the discretionary judgment of the states that already hold recognized status, concentrating a veto over new entrants in the hands of incumbents (especially the P5 and regional hegemons).
% ABSENT_VOICES: Unrecognized and de facto polities have no vote or standing in the bodies (UN General Assembly admission votes, Security Council recommendations) that decide their own status; their populations are structurally excluded from the recognition-granting community whose decision governs their legal existence.
% DISAPPEARANCE_RATIONALE: If constitutive recognition were abolished overnight in favor of pure declaratory effect, entities meeting the four objective Montevideo criteria (territory, population, government, capacity for external relations) would immediately hold treaty capacity and could seek IMF/World Bank membership and diplomatic immunity without waiting on the political assent of incumbent states — several long-standing de facto states would acquire full international legal personality essentially at once, and incumbent states would lose their current veto over new entrants.
% FOUNDING_PROBLEM: In the early-to-mid twentieth century, states needed a stable, mutually intelligible standard for deciding which entities counted as fellow sovereigns for purposes of treaty-making, diplomatic exchange, and membership in the emerging system of international organizations, replacing ad hoc, unilateral, and inconsistent dynastic-era recognition practices.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars outside the recognition-granting states (e.g., commentary tracing the Tinoco arbitration and post-1945 ICJ jurisprudence) attest that the underlying coordination problem — mutual intelligibility of who counts as a state — was substantially solved by the objective-criteria framework decades ago, and that the persistence of a recognition veto now functions primarily to preserve incumbents' geopolitical leverage rather than to solve any live coordination problem; the P5 and regional hegemons themselves attest the problem remains live, citing risks of legal chaos from unrestrained unilateral secession.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high-moderate (0.71 at interval end) because the constitutive reading converts a factual governance record into a legal non-status unless and until politically interested incumbents consent — the gap between 'functions as a state' and 'is recognized as one' is precisely where value (treaty capacity, IFI membership, immunity) is withheld from populations who have already borne the costs of building the governance capacity the criteria describe. Suppression is authored higher (0.78) because the mechanism does not merely fail to help unrecognized polities — it actively forecloses their access to the instruments (UN membership, bilateral treaties, multilateral lending) that would let them exit the unrecognized category through their own governance performance alone; recognition remains a political act regardless of merit. Theater ratio is moderate (0.32): objective-criteria review by diplomatic services is a real, functioning filter, but a growing share of stated rationale is retrospective justification for recognition decisions actually driven by patron-state alignment.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seats (P5, regional hegemons, existing recognized states), the constitutive requirement reads as prudent gatekeeping against destabilizing proliferation of unstable claimants — a genuine coordination function. From the unrecognized-polity seats, the identical structure reads as an arbitrary political veto exercised by parties with no stake in, and often an active interest against, the claimant's success. The engine computes these as structurally different experiences of the same requirement from the authored power/exit data; the divergence is not resolved by picking a side.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (permanent Security Council members, regional hegemons, existing recognized states, patron states) hold arbitrage-grade or mobile exit and institutional/organized/powerful power — they can extend or withhold recognition as instrumental foreign policy without bearing symmetric cost, placing them near the beneficiary end of directionality. Victims (unrecognized polities, de facto states, aspirant secessionist populations) are powerless and trapped: they have already built the governance capacity the objective criteria describe and have no lever to convert that capacity into legal personality without the very incumbents' consent that the constitutive doctrine makes decisive. This is the textbook derivation the engine performs from beneficiary/victim declarations plus exit options — no override is needed here because the structural asymmetry is stark and directly captured by the declared exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing a stable, mutually intelligible standard replacing ad hoc unilateral recognition practices — is largely solved: the four objective criteria are now widely understood and routinely applied evidentially even by states that ultimately withhold recognition for political reasons. Yet the constitutive requirement persists past that solved problem, now functioning chiefly to preserve incumbent states' discretionary leverage over new entrants. Classifying this as tangled_rope rather than snare or mountain matters: it is not pure extraction (a genuine coordination problem — avoiding recognition chaos — is real and would recur if abolished carelessly) and it is not a natural law (a declaratory or hybrid regime is coherently conceivable and is in fact actively defended by rival readings of the same kernel). The tangled_rope classification requires both the coordination function and the asymmetric extraction to be genuinely present, which the founding-problem mismatch (status=contested, corroboration split between outside scholars and interested incumbents) evidences directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_legal_fact,
    'Is the requirement of recognition genuinely constitutive of statehood as a matter of international law, or is recognition merely evidentiary/declaratory of a state that already exists once the objective criteria are met?',
    'Systematic review of ICJ and arbitral jurisprudence (e.g., the Badinter Commission opinions, the Tinoco arbitration, state practice on premature and delayed recognition) to determine whether tribunals treat non-recognition as denying legal personality outright or merely as withholding certain privileges from an entity whose statehood is otherwise treated as existing.',
    'If the declaratory view is legally correct, the constitutive reading''s extraction is better characterized as a political practice riding on top of settled law rather than the law itself — shifting this story''s classification toward snare (extraction dressed as doctrine) rather than tangled_rope (doctrine with a real coordination function). If the constitutive view is legally correct, the coordination function claimed here is genuinely load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_legal_fact, conceptual, 'Whether recognition creates or merely evidences statehood under international law.').

omega_variable(
    recognition_veto_political_capture,
    'To what extent is non-recognition of specific de facto states driven by genuine concerns about the objective criteria (governance capacity, territorial control) versus by patron-state rivalry entirely unrelated to the claimant''s own conduct?',
    'Comparative case study of recognition patterns across de facto states with similar governance profiles but different patron alignments (e.g., contrasting outcomes where objective indicators are held roughly constant but geopolitical alignment differs).',
    'A finding that recognition outcomes track patron rivalry more than governance indicators would strengthen the extraction reading and support reclassification pressure toward snare; a finding that recognition tracks governance indicators fairly closely would support the coordination framing incumbents offer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_veto_political_capture, empirical, 'Whether non-recognition tracks objective governance failure or great-power rivalry.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all three readings (constitutive, declaratory, hybrid) are actively defended by different legal traditions and state practice, is there a principled basis for treating the constitutive reading as the operative one in this story, or does the framing choice itself carry normative weight?',
    'Document the reading selection explicitly (done here via kernel_context) and cross-reference against the sibling stories'' ε and victim sets rather than resolving which reading is ''correct'' within this file.',
    'Choosing to author the constitutive reading as the primary lens for this file determines which populations appear as victims (unrecognized polities) and which appear as beneficiaries (incumbent recognizers); the declaratory reading would produce a nearly empty victim set for the same underlying facts, since it treats recognition as non-determinative of legal status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Framing under-determination across the three sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mont_tr_t16, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(mont_tr_t32, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(mont_tr_t48, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 48, 0.28).
narrative_ontology:measurement(mont_tr_t64, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 64, 0.3).
narrative_ontology:measurement(mont_tr_t80, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 80, 0.32).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(mont_be_t16, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(mont_be_t32, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(mont_be_t48, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(mont_be_t64, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 64, 0.68).
narrative_ontology:measurement(mont_be_t80, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 80, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mont_su_t16, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(mont_su_t32, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(mont_su_t48, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement(mont_su_t64, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 64, 0.75).
narrative_ontology:measurement(mont_su_t80, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 80, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__constitutive_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the montevideo_statehood_criteria kernel, decomposed per the ε-invariance principle: constitutive_reading (this file, ε=0.71, tangled_rope), declaratory_reading (separate file, expected lower ε — objective criteria alone as legal fact, minimal incumbent veto), and hybrid_reading (separate file, expected intermediate ε with a narrower and differently-composed victim set — only claimants failing normative legitimacy conditions are excluded). Each reading instantiates a structurally distinct constraint with its own beneficiary/victim structure; they are linked here via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
