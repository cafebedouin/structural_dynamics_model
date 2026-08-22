% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Restrictive Sovereignty Reading of the Refugee Convention (Minimum-Floor / Maximum-Discretion Doctrine)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The restrictive sovereignty reading operates the Refugee Convention as a
 *   minimum floor: protection is owed only where an applicant proves
 *   individualized persecution by the state (or with the state's awareness),
 *   and destination states retain maximum discretion above that floor —
 *   admissibility screening, offshore processing, safe-third-country
 *   transfer, and exclusion of generalized-violence and non-state-persecution
 *   claims. The reading has a genuine coordination function: the narrow,
 *   provable threshold is what made near-universal ratification possible and
 *   keeps individualized adjudication administrable at scale. It also carries
 *   a real cost side: the classes it excludes — most of the modern displaced
 *   population — bear detention, offshore confinement, and refoulement risk
 *   through the same structure that subsidizes destination-state migration
 *   control. This story is ONE READING of the refugee_convention_text kernel
 *   and is deliberately kept epsilon-invariant: the
 *   expansive_humanitarian_reading and procedural_integrity_reading stories
 *   author different constraints over the same text, with different victim
 *   sets and different epsilon values, and are linked here via
 *   network.affects_constraints. The epsilon referent is this reading's
 *   standing arrangement as this story assesses it — never the arrangement
 *   the sibling readings would create.
 *
 * KEY AGENTS:
 *   - destination_states: agenda-setter and primary beneficiary (institutional/mobile) — adopt and enforce the reading, collect migration control and reduced obligations
 *   - asylum_seekers_fleeing_generalized_violence: primary payer (powerless/trapped) — excluded by the individualization threshold, bear detention and refoulement risk
 *   - non_state_persecution_survivors: primary payer (powerless/trapped) — excluded by the state-awareness requirement
 *   - classic_persecution_claimants: beneficiary (powerless/trapped) — the narrow channel protects exactly their claim-type
 *   - border_enforcement_apparatus: secondary beneficiary (organized/constrained) — budgets and mandates scale with the exclusion workload
 *   - offshore_host_states: payer (moderate/constrained) — bear confinement costs under paid transfer arrangements
 *   - unhcr: analytical observer (institutional/analytical) — supervises the Convention, holds the expansive reading, no vote in destination adjudication
 *   - refugee_law_ngo_litigators: excluded (organized/mobile) — would contest the reading but lack standing in screening and offshore fora
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.72).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Restrictive Sovereignty Reading of the Refugee Convention (Minimum-Floor / Maximum-Discretion Doctrine)").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'c71b26be-83a4-4399-af0e-3105cf01008e').
narrative_ontology:cs_kernel_codification('c71b26be-83a4-4399-af0e-3105cf01008e', fixed_text).
narrative_ontology:cs_authority_grounding('c71b26be-83a4-4399-af0e-3105cf01008e', lineage).
narrative_ontology:cs_interpretation_layer_present('c71b26be-83a4-4399-af0e-3105cf01008e').
narrative_ontology:cs_reading_relation('c71b26be-83a4-4399-af0e-3105cf01008e', refugee_convention_text__expansive_humanitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('c71b26be-83a4-4399-af0e-3105cf01008e', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('c71b26be-83a4-4399-af0e-3105cf01008e', foundational, sovereign_discretion_above_floor).
narrative_ontology:cs_axiom_status(sovereign_discretion_above_floor, holdable).
narrative_ontology:cs_axiom_grounding('c71b26be-83a4-4399-af0e-3105cf01008e', sovereign_discretion_above_floor, conventional).
narrative_ontology:cs_axiom('c71b26be-83a4-4399-af0e-3105cf01008e', foundational, individualized_persecution_proof_required).
narrative_ontology:cs_axiom_status(individualized_persecution_proof_required, holdable).
narrative_ontology:cs_axiom_grounding('c71b26be-83a4-4399-af0e-3105cf01008e', individualized_persecution_proof_required, conventional).
narrative_ontology:cs_axiom('c71b26be-83a4-4399-af0e-3105cf01008e', secondary, psg_limited_to_immutable_state_aware_traits).
narrative_ontology:cs_axiom_status(psg_limited_to_immutable_state_aware_traits, holdable).
narrative_ontology:cs_axiom_grounding('c71b26be-83a4-4399-af0e-3105cf01008e', psg_limited_to_immutable_state_aware_traits, conventional).
narrative_ontology:cs_reference_frame('c71b26be-83a4-4399-af0e-3105cf01008e', ratification_bargain_floor).
narrative_ontology:cs_drift_state('c71b26be-83a4-4399-af0e-3105cf01008e', contemporary_mass_displacement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c71b26be-83a4-4399-af0e-3105cf01008e', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, destination_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, classic_persecution_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_fleeing_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_survivors).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, offshore_host_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt and administer the restrictive reading through their courts, immigration tribunals, and executive screening processes. Collect reduced protection obligations, migration control, and the domestic political credit that comes with tight borders. They can shift enforcement offshore, reinterpret doctrine through their own judiciaries, or ultimately denounce the treaty, so their exit from the arrangement is unusually cheap for parties inside a multilateral regime.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_states, agenda_setter,
    institutional, generational, mobile, global).

% Flee civil wars, indiscriminate bombardment, and collapsed states. Their claims fail the individualized-persecution threshold regardless of the danger they face, because no one targeted them personally for a Convention reason. They wait in detention or camps, face transfer to third countries, and carry the risk of return to the danger they fled. Leaving the arrangement would require a protection claim shaped to the narrow frame, which their circumstances cannot produce.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_fleeing_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Persecuted by militias, cartels, gangs, or family members in states that are unable or unwilling to protect them. The state-awareness and state-action requirements of the narrow reading exclude them even where the harm is severe and the state's failure is documented. They cannot go home and cannot reposition their claim inside the protected class.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_survivors, payer,
    powerless, immediate, trapped, global).

% Dissidents, journalists, and minorities targeted individually by their own state, with the state's knowledge documented. Their claims fit the narrow frame cleanly: individualized proof, state-directed harm, immutable characteristics the persecutor knows. The protection channel the reading preserves is built around exactly their claim-type, and the narrowness is defended as keeping the system credible for them.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, classic_persecution_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Screening units, detention systems, and offshore processing contractors whose budgets, headcount, and mandates scale with the exclusion workload the narrow threshold generates. They operationalize admissibility decisions day to day and collect the appropriations that the enforcement posture justifies. Their mandate is created and defunded by the destination states, so they cannot walk away from it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_apparatus, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_apparatus, agenda_setter).

% Third countries paid to host processing centers or accept transferred applicants. They bear the operational cost, the reputational exposure, and the human-rights litigation risk of confinement arrangements they did not design, in exchange for aid revenue and diplomatic favor. Exiting means losing the payments and the relationship with the destination states.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, offshore_host_states, payer,
    moderate, biographical, constrained, national).

% Holds supervisory status under the Convention and advocates the broad humanitarian interpretation in its guidelines and interventions. It publishes, advises, and litigates as amicus but holds no vote in destination-state adjudication, and it observes the widening gap between its supervisory reading and state practice without the power to close it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr, observer,
    institutional, generational, analytical, global).

% Scholars, NGO lawyers, and refugee-led organizations who would contest the narrow reading in the fora where it hardens. In executive screening processes and offshore processing zones they often lack standing entirely; their objections surface in commentary, treaty-body submissions, and occasional strategic litigation rather than in the rooms where admissibility rules are written.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, refugee_law_ngo_litigators, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, destination_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates state participation in a shared minimum-protection standard: by fixing a narrow, provable, individualized threshold, it gave destination states a common floor they could ratify in 1951 without ceding immigration control, and it gives adjudicators a consistent, administrable evidentiary standard for deciding claims at scale. The floor design is what kept near-universal participation possible.
% TRANSFER_FUNCTION: Moves protection obligations away from destination states: the cost of non-protection lands on the excluded claimants themselves (detention, offshore confinement, refoulement risk), part of it is shifted to paid third countries hosting offshore arrangements, and discretion over who is protected moves from adjudicators to executives through admissibility screening.
% ABSENT_VOICES: The claimants excluded by the threshold have no seat in the interpretive fora where the reading hardens: executive screening processes and offshore zones sit outside ordinary judicial review, UNHCR holds supervisory status but no vote, and refugee-led organizations are rarely consulted in doctrinal development. The unanimity of destination-state practice partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished overnight, admissibility screening regimes would lose their doctrinal basis, offshore processing and safe-third-country transfers would face immediate successful challenge, grant rates for generalized-violence and non-state-persecution claims would rise sharply, and destination states would confront a choice between administering the broader obligation and openly denouncing the Convention. The externalized-border architecture built on the reading would reorganize within years.
% FOUNDING_PROBLEM: The 1951 drafters faced states unwilling to accept open-ended protection obligations. The Convention was drafted around the individualized, state-directed persecution of the Nazi era, with sovereignty-preserving design — no obligation to grant asylum, no suspension of immigration control, a narrow provable definition — as the price of near-universal ratification.
% FOUNDING_PROBLEM_CORROBORATION: Travaux préparatoires and ratification histories attest the narrow-floor bargain from outside any current beneficiary seat. UNHCR supervisory reports and the academic treaty literature attest that the founding problem's terms have shifted: mass flight from generalized violence and non-state persecution now dominates displacement, which the 1951 frame did not contemplate. Regional instruments corroborate the contest directly — the 1969 OAU Convention and the 1984 Cartagena Declaration both adopted broader definitions and still achieved wide ratification, which disputes the claim that narrowness was necessary to hold a regime together. No single source settles whether the floor must stay narrow today; the record shows both halves of the dispute are attested.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.68 because the reading's operative content is the exclusion: the majority of the modern displaced population flees generalized violence or non-state persecution, and the narrow threshold removes them from protection regardless of the severity of their situation, while a shrinking class of classic individualized claimants passes. Suppression is 0.72 and structural, not internalized: admissibility screening, safe-third-country rules, offshore processing, and pushback practices are legal architecture the reading's permissiveness enables — the machinery is external to the claimant and does not depend on the claimant's beliefs. Theater is 0.38: the individualized adjudication machinery is real for the claims it admits, but state-awareness inquiries and immutable-characteristics tests increasingly function as outcome-driven gatekeeping on the excluded classes. Accessibility collapse is 0.45 — deliberately moderate: alternatives persist and compete (complementary protection, temporary protection regimes, regional instruments with broader definitions, NGO litigation, and the two sibling readings themselves remain live), so this reading does not collapse alternatives; it contests them. Resistance is 0.62: UNHCR supervisory interpretation, human rights treaty bodies, a substantial share of the scholarly literature, and several high courts actively resist, and the reading holds in destination-state practice against that sustained contest. All three metric series run on one shared time grid (points 0-42) so every metric is authored at every examined time point; the shared rise tracks the shift in displacement character — as flight from generalized violence came to dominate, the gap between the reading's protected class and actual protection need widened, raising both the exclusion cost and the enforcement machinery needed to hold the threshold in place.
 *
 * PERSPECTIVAL GAP:
 *   The destination-state seat and the payer seats should compute different types from the same structural data. From the destination-state seat (institutional power, mobile exit, agenda control), the reading is the price of the regime's existence: a manageable evidentiary standard that keeps states inside a multilateral framework they could otherwise leave, with extraction experienced as negligible because the state collects the gains. From the payer seats (powerless, trapped), the same structure is a denial of protection with refoulement risk attached — the coordination they are asked to participate in is one they cannot exit and did not agree to. The border_enforcement_apparatus seat benefits from the exclusion workload without bearing its human costs, and the offshore_host_states seat bears operational costs without setting the agenda — a same-regime asymmetry the derivation captures from exit options and role rather than nominal power. UNHCR's observer seat registers the divergence as a supervisory gap it documents but cannot close.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly onto the structural relationships. destination_states sit at the full-beneficiary end: they set the reading, enforce it, and collect migration control and electoral credit from it. border_enforcement_apparatus is a genuine but secondary beneficiary — appropriations scale with the workload, though it carries some political exposure. classic_persecution_claimants are genuinely subsidized: the narrow channel exists for their claim-type, and the reading's defenders cite their cases as proof the system works. The payer seats sit toward the full-target end, and their trapped exit pushes them further: asylum seekers fleeing generalized violence and non-state persecution survivors cannot return home, cannot reposition their claims inside the protected class, and bear the arrangement's costs directly. offshore_host_states are payers with moderate power and constrained exit — they bear confinement costs under paid arrangements they did not design. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct d values for every seat, and the override mechanism (keyed to power atoms) could not distinguish the beneficiary and payer seats that share the powerless power atom without mis-correcting one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing state participation through a narrow floor — was solved by ratification, but whether it remains solved is exactly the kernel contest: destination states attest the floor must stay narrow or states exit; UNHCR, treaty bodies, and the OAU/Cartagena record attest that broader definitions ratify fine and that the founding problem has transformed into mass-displacement management. The tangled_rope classification prevents mislabeling in both directions. A pure-snare classification would erase the genuine coordination function: the floor design did solve the 1951 ratification problem, and individualized adjudication does solve a real administrability problem — those are not cover stories. A pure-rope classification would erase the excluded classes who pay through the same structure that coordinates the states. The mandatrophy watch point is atrophy of the coordination half: as the protected class shrinks relative to displacement need, the reading's coordination function degrades into ratification theater — states invoking a bargain whose original parties and problem have both largely passed — and the theater_ratio series in the measurements is the instrument that tracks exactly that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is the restrictive_sovereignty_reading of the refugee_convention_text kernel. Is the narrow victim set — individualized, state-aware persecution only — the Convention''s actual structural content, or an artifact of reading the ratification bargain as a ceiling on protection rather than a floor beneath which states retain discretion to do more?',
    'Comparative adjudication across jurisdictions that have adopted different readings of the identical text; ICJ and treaty-body determinations on the Convention''s ordinary meaning; systematic analysis of the travaux préparatoires against the text''s subsequent practice.',
    'If the expansive reading is the better reading of the same text, this constraint''s victim set is under-inclusive by interpretive choice and the exclusion it produces is rent on a misreading; if the restrictive reading is correct, the measured costs are the price of the regime''s actual terms and the sibling readings are aspirations, not readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Which reading of the fixed Convention text the narrow-floor arrangement actually instantiates.').

omega_variable(
    ratification_bargain_necessity,
    'Was the narrow, individualized floor actually necessary to secure near-universal ratification in 1951, or would states have ratified a broader mandate — making the narrowness a choice that benefits destination states rather than a coordination cost?',
    'Counterfactual treaty-history analysis plus the natural experiment already run: the 1969 OAU Convention and the 1984 Cartagena Declaration both broadened the refugee definition and still achieved wide ratification across their regions. Compare ratification rates, reservation patterns, and actual protection practice under broad versus narrow definitions.',
    'If broader definitions demonstrably ratify and function, the narrowness is extraction layered onto coordination rather than the coordination cost itself, and the reading''s effective extractiveness is understated by the floor''s necessity claim; if the regional instruments'' broader definitions produced thinner compliance, the necessity claim gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_bargain_necessity, empirical, 'Whether the floor''s narrowness was a binding coordination constraint or a beneficiary-favoring design choice.').

omega_variable(
    reading_vs_enforcement_attribution,
    'How much of the measured suppression and extraction belongs to the reading itself, versus enforcement practices (offshore processing, pushbacks, detention regimes) that the reading merely permits but does not require?',
    'Compare protection outcomes and enforcement intensity across destination states that share the restrictive reading but differ in enforcement posture; if outcomes diverge widely under the same reading, much of the measured harm belongs to separate enforcement constraints; if outcomes converge, the reading''s permissiveness is doing the work.',
    'If the reading only enables the enforcement machinery, part of the authored suppression should be attributed to distinct enforcement-constraint stories and this story''s epsilon should fall; if the permissiveness is the enabling condition without which the machinery could not operate, the attribution here is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_enforcement_attribution, conceptual, 'Attribution boundary between the interpretive constraint and the enforcement practices it permits.').

omega_variable(
    psg_immutability_principle,
    'Is the immutable-characteristics limit on ''particular social group'', together with the state-awareness requirement, a principled interpretive line, or an ad hoc instrument whose application tracks the excluded claim classes (gender, LGBTQ+, clan-based persecution) rather than any coherent doctrinal criterion?',
    'Track doctrinal coherence across the full population of adjudicated PSG claims: whether outcomes correlate with the stated criterion (immutability, social perception, state awareness) or with claim-class identity; compare against jurisdictions adopting social-perception approaches.',
    'If the limit is ad hoc, the PSG gate is exclusion dressed as doctrine, the theater component of this constraint is higher than authored, and the effective victim set is larger than the doctrine admits; if the limit is principled, part of the measured exclusion is a genuine administrability cost of any workable definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psg_immutability_principle, conceptual, 'Whether the PSG immutability limit is doctrine or gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(refu_tr_t7, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 7, 0.24).
narrative_ontology:measurement(refu_tr_t14, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 14, 0.27).
narrative_ontology:measurement(refu_tr_t21, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 21, 0.31).
narrative_ontology:measurement(refu_tr_t28, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 28, 0.33).
narrative_ontology:measurement(refu_tr_t35, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 35, 0.36).
narrative_ontology:measurement(refu_tr_t42, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 42, 0.38).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(refu_be_t7, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(refu_be_t14, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 14, 0.56).
narrative_ontology:measurement(refu_be_t21, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 21, 0.6).
narrative_ontology:measurement(refu_be_t28, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 28, 0.63).
narrative_ontology:measurement(refu_be_t35, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 35, 0.66).
narrative_ontology:measurement(refu_be_t42, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 42, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(refu_su_t7, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 7, 0.5).
narrative_ontology:measurement(refu_su_t14, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 14, 0.55).
narrative_ontology:measurement(refu_su_t21, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 21, 0.61).
narrative_ontology:measurement(refu_su_t28, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(refu_su_t35, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 35, 0.69).
narrative_ontology:measurement(refu_su_t42, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 42, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the refugee_convention_text kernel (one fixed treaty text) decomposes into three reading-constraints per the epsilon-invariance principle. This story (restrictive_sovereignty_reading) authors epsilon over the narrow-floor arrangement: high admissibility screening, offshore processing permissible, generalized violence and non-state persecution excluded, victim set = the excluded claim classes. The expansive_humanitarian_reading story authors a different constraint over the same text — an unbendable humanitarian mandate with a much broader protected class and correspondingly different epsilon and victim structure. The procedural_integrity_reading story authors a third: process integrity non-negotiable, threshold flexible. The restrictive and expansive readings directly contradict on the protection trigger and cannot coexist within a single adjudicative framework (forecloses); the restrictive reading's admissibility and offshore architecture structurally erodes the operating conditions of the procedural reading without logically ruling out a narrow-but-fair framework (influences). Each story links the others via network.affects_constraints; no story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
