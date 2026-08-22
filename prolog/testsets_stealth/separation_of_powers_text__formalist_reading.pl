% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Separation-of-Powers Boundary: The Non-Delegation Rule
 *   domain: constitutional law/political theory/administrative law
 *
 * SUMMARY:
 *   This story instantiates the FORMALIST READING of the separation-of-powers
 *   kernel: the claim that Articles I-III establish strict, impermeable
 *   boundaries among the branches, and that Congress therefore cannot
 *   transfer legislative authority to administrative agencies. As a standing
 *   constraint, the rule operates through judicial invalidation: delegations
 *   lacking intelligible-and-limited standards fall, agency rulemaking
 *   authority is voided, and regulatory gaps close unless Congress legislates
 *   in exhaustive detail. The structural delta this reading produces is
 *   distinctive: administrative agencies enter the victim set wholesale,
 *   national regulatory capacity contracts drastically, and the alternative
 *   governance technology (expert administration under general statutory
 *   mandates) is suppressed as a constitutional category rather than merely
 *   disfavored. The constraint is part of a decomposed family: the colloquial
 *   label 'separation of powers' conflates at least three structurally
 *   distinct claims (this formalist boundary rule, the functionalist
 *   flexibility framework, and the unitary-executive concentration claim),
 *   each with its own epsilon, beneficiary structure, and victim set; the
 *   siblings are separate constraint stories linked through the network
 *   surface, not folded into this one. Interval units are years since 1935
 *   (the last sustained enforcement era).
 *
 * KEY AGENTS:
 *   - - federal_judiciary: Agenda-setter and beneficiary (institutional / identity_locked) — administers the boundary by deciding which delegations survive; captures adjudicative supremacy over the scope of federal power
 *   - - regulated_industries: Primary material beneficiary (powerful / arbitrage) — finances constitutional challenges and collects deregulatory relief when delegations fall
 *   - - federal_administrative_agencies: Primary target (institutional / trapped) — delegated rulemaking authority voided; cannot exit statutory mandates that the constraint empties of implementing power
 *   - - regulation_dependent_public: Target (powerless / trapped) — bears the loss of air, water, food, drug, workplace, and financial protections previously supplied by invalidated rules
 *   - - united_states_congress: Dual-positioned (institutional / constrained) — formally reclaims exclusive legislative power while practically bearing the cost of either micro-legislating or accepting regulatory voids
 *   - - agency_subject_matter_experts: Excluded voice (moderate / constrained) — the scientists and economists whose drafting function the constraint voids; they hold no seat in the constitutional conversation that extinguishes their role
 *   - - constitutional_legal_academy: Analytical observer (analytical / analytical) — maps the stakes without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.72).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.78).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Separation-of-Powers Boundary: The Non-Delegation Rule").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional law/political theory/administrative law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, 'ba7731ad-a921-408e-b4bc-2bdf097acca4').
narrative_ontology:cs_kernel_codification('ba7731ad-a921-408e-b4bc-2bdf097acca4', fixed_text).
narrative_ontology:cs_authority_grounding('ba7731ad-a921-408e-b4bc-2bdf097acca4', lineage).
narrative_ontology:cs_interpretation_layer_present('ba7731ad-a921-408e-b4bc-2bdf097acca4').
narrative_ontology:cs_reading_relation('ba7731ad-a921-408e-b4bc-2bdf097acca4', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ba7731ad-a921-408e-b4bc-2bdf097acca4', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('ba7731ad-a921-408e-b4bc-2bdf097acca4', foundational, legislative_power_nontransferable).
narrative_ontology:cs_axiom_status(legislative_power_nontransferable, holdable).
narrative_ontology:cs_axiom_grounding('ba7731ad-a921-408e-b4bc-2bdf097acca4', legislative_power_nontransferable, conventional).
narrative_ontology:cs_axiom('ba7731ad-a921-408e-b4bc-2bdf097acca4', secondary, liberty_requires_impermeable_boundaries).
narrative_ontology:cs_axiom_status(liberty_requires_impermeable_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('ba7731ad-a921-408e-b4bc-2bdf097acca4', liberty_requires_impermeable_boundaries, deontological).
narrative_ontology:cs_reference_frame('ba7731ad-a921-408e-b4bc-2bdf097acca4', founding_separated_powers_allocation).
narrative_ontology:cs_drift_state('ba7731ad-a921-408e-b4bc-2bdf097acca4', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ba7731ad-a921-408e-b4bc-2bdf097acca4', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, united_states_congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_industries).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, federal_administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulation_dependent_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, united_states_congress).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, anti_aggrandizement_principle).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, congressional_primacy_in_lawmaking).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which statutory delegations survive and which fall, and in doing so fixes the boundary between lawmaking and administration. Each invalidation expands the judiciary's supervisory jurisdiction over the regulatory state. Its members are selected into a guardianship self-conception in which policing the boundary is the institution's defining duty; stepping back from that role is not an available move from inside it.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, federal_judiciary, beneficiary).

% Finances constitutional litigation challenging the statutes under which agencies regulate them. When a delegation falls, compliance obligations, reporting duties, and exposure to enforcement vanish without any compensating payment. Capital and legal talent move freely among forums, jurisdictions, and future challenge campaigns; the constraint costs them nothing they do not already spend voluntarily.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% Administer statutes on the strength of delegated rulemaking authority. Under the constraint that authority is void: the statutory mandate survives, the power to execute it does not. Staff, data systems, and procedures built over generations lose their legal outlet. There is no exit — an agency cannot relocate its function, recharter itself, or take its expertise to a market; it can only shrink toward advisory and enforcement-of-remaining-text roles.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, federal_administrative_agencies, payer,
    institutional, generational, trapped, national).

% Relies on agency rules for air quality, water safety, drug approval, food inspection, workplace safety, and financial fraud prevention. When delegations fall, the protections lapse and no market or private substitute reproduces them at scale. Individual members cannot exit the exposure and have negligible individual voice; their only lever is coalition through public-interest organizations litigating as intervenors, which arrives late and underfunded relative to industry challengers.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulation_dependent_public, payer,
    powerless, generational, trapped, national).

% Formally reclaims exclusive possession of legislative power: every invalidated delegation returns lawmaking authority to the chamber the Constitution vested it in. Practically, members must then either write statutes of impossible technical detail or leave the problem unaddressed, because electoral time horizons, committee bottlenecks, and knowledge limits make detailed legislation unavailable at the volume modern governance demands. The benefit and the burden land on the same institution through different seats.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, united_states_congress, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, united_states_congress, payer).

% Scientists, epidemiologists, economists, and engineers whose careers are the drafting function the constraint voids. They possess the operational knowledge that detailed congressional legislation would require and the direct testimony that it cannot be replicated, but they hold no seat in the doctrinal conversation that decides their function's fate; they appear only as exhibits in others' briefs.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, agency_subject_matter_experts, excluded,
    moderate, biographical, constrained, national).

% Scholars and theorists who map the boundary question, reconstruct founding-era practice, and forecast the downstream effects of each reading. They collect no rents from the constraint's operation and bear none of its costs; their output shapes the terms in which the other seats argue.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, constitutional_legal_academy, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: keeping decisions that bind everyone in the hands of the branch that answers to everyone, preventing any branch from accumulating lawmaking plus execution plus adjudication, and giving citizens a single identifiable location where the rules that bind them are made. Stated without evaluation of whether the rigid form is the right solution.
% TRANSFER_FUNCTION: Moves rule-writing authority from administrative agencies back to Congress; moves adjudicative power over the scope of federal regulatory authority to the courts; moves compliance relief to regulated industries that finance the challenges; moves protective coverage away from the regulation-dependent public.
% ABSENT_VOICES: Agency subject-matter experts and the regulation-dependent public would object if seated — the former with testimony that detailed legislation cannot reproduce technical rulemaking, the latter with the concrete inventory of protections that lapse when delegations fall. Both are structurally outside the conversation: constitutional argument is conducted among judges, litigants, and scholars, and the people who operate or depend on the invalidated machinery appear only through counsel retained by others.
% DISAPPEARANCE_RATIONALE: The parties dispute which world follows. On the formalist account, the constraint's disappearance abandons accountable lawmaking: Congress continues offloading hard questions, power concentrates in unelected administrations, and the constitutional design fails by attrition — the world rearranges badly. On the functionalist account, nothing rearranges: the doctrine lay dormant for five decades while the administrative state operated, and its removal merely restores the pre-revival equilibrium of deferential review. The empirical record supports both descriptions at different phases of the lifecycle, which is why the verdict is contested rather than resolved.
% FOUNDING_PROBLEM: The founding generation's problem of concentrated power: preventing any single actor or branch from combining lawmaking, execution, and judgment, and ensuring that rules binding the public are made by representatives answerable at election. The non-delegation rule is the formalist answer to the specific sub-problem of Congress transferring that lawmaking function away.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem is corroborated from outside the benefiting parties: political historians and constitutional scholars across the functionalist-formalist divide attest that the founding generation genuinely feared aggrandizement and that the concern remains real (the Federalist papers' anti-aggrandizement arguments are corroborated by the historical record of the period, not merely by the constraint's beneficiaries). Functionalist jurists — adversaries of this reading — expressly concede the problem's reality while disputing the rigid remedy, which is corroboration of the problem from the opposing camp. No comparable external corroboration exists for the claim that impermeable boundaries were the founders' exclusive understanding; that specific historical assertion is contested within the scholarly literature and is flagged in the originalist_historical_record omega.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, contested).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the rule's operation destroys accumulated governance capital: decades of agency rulemaking, expertise, and protective coverage are voided at a stroke, and the replacement (detailed congressional legislation) is structurally unavailable at scale due to collective-action and knowledge limits. Suppression is higher still (0.78) because the constraint does not merely tax the alternative — it abolishes it as a constitutional category; administrative governance is not made expensive but illegitimate, which is suppression of an entire governance mode rather than of a competitor within a mode. Accessibility_collapse is 0.68: once the rule binds, alternatives (interstate compacts, state regulation, private standard-setting) survive only partially and cannot substitute for national regulatory capacity. Resistance is 0.70: the doctrine faces entrenched functionalist precedent, an administrative apparatus with survival incentives, and political coalitions built on regulatory protections. Theater_ratio is 0.28 at interval end and falling — the accountability rhetoric is increasingly backed by real enforcement — but the series shows the constraint's full lifecycle: brief real enforcement (t=0, the 1935 era), a long dormancy in which the doctrine was invoked rhetorically while rarely enforced (theater peaking at 0.58 around t=45), and a revival in which theater converts back into function. The three metric series run on one shared time grid (every tracked metric authored at every examined point) so drift dating is not contaminated by scalar substitution. Suppression here is structural throughout — constitutional invalidation removes the alternative externally; there is no meaningful internalized component, so no structural-vs-internalized omega is required.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the judiciary's agenda-setter seat the constraint is constitutional fidelity: the Court is not extracting but restoring a design, and the coordination benefit (accountable lawmaking) is vivid from inside the guardianship role. From the agency seat the same operation is annihilation of function without exit — the mandate remains, the authority to execute it does not. From the regulated-industry seat the constraint is pure windfall: obligations vanish at the price of litigation the industry chose to fund. Congress is internally split across its own seats: the chamber-as-institution formally gains power while members-as-electoral-actors lose the ability to delegate hard problems, which is why its directionality sits mid-scale despite nominal beneficiary status. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: the judiciary collects adjudicative power (low d, amplified by identity_lock on the guardianship role); regulated industries collect compliance relief with arbitrage-grade mobility (lowest d); Congress formally collects restored legislative authority (moderate d — the benefit is real but offset by the capacity cost carried through its secondary payer role). Victim declarations: agencies bear full-target directionality (trapped exit, institutional power that cannot be redeployed outside the emptied mandate); the regulation-dependent public bears high d with trapped exit and negligible individual leverage — its only route is coalition through public-interest organizations, which raises its effective power above raw powerlessness but far below its adversaries'. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options produces the correct qualitative ordering for every seat, and the schema's override mechanism is keyed by power atom rather than agent, which is too coarse to improve on the derivation here (an 'institutional' override would simultaneously distort the judiciary, agencies, and Congress). Congress's dual position is carried by its secondary_role and this commentary rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing aggrandizement and keeping lawmaking in elected hands — remains live, so the constraint is not mandatrophy-resolved and none of its maintenance is purely theatrical at interval end. But the dormancy era demonstrates the failure mode this classification guards against: for roughly five decades the doctrine persisted as rhetorical invocation (theater_ratio above 0.5) while performing almost no function — a piton-shaped episode inside a tangled-rope lifecycle. The revival converted performance back into enforcement, which is why the end-state theater_ratio falls. Conversely, the classification prevents the opposite mislabel: reading the doctrine's high extraction and suppression as pure snare would erase its genuine coordination function (no serious participant denies that accountable lawmaking and anti-aggrandizement are real collective goods the arrangement serves); reading it as pure rope would erase the asymmetric extraction borne by agencies and the regulation-dependent public through the same structure that coordinates. The tangled-rope classification holds both facts: coordination through congressional primacy, extraction through the destruction of the administrative alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the separation_of_powers_text kernel (the formalist_reading). How would the classification change if instantiated instead as the functionalist_reading, under which overlapping authority and intelligible-principle delegation are permitted?',
    'Generate the sibling stories (separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading) and compare computed types, epsilon, and victim sets across the kernel.',
    'Under the functionalist reading, administrative agencies drop out of the victim set entirely, epsilon falls sharply, and the computed type plausibly moves toward rope; the entire extraction profile of this story is contingent on the formalist premise that boundaries are impermeable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Classification contingency on kernel-reading selection: the victim set and epsilon are properties of this reading, not of the separation-of-powers kernel as such.').

omega_variable(
    nondelegation_entrenchment_trajectory,
    'Will the formalist non-delegation rule entrench as governing law (regularly deployed to invalidate delegations), or remain a minority position invoked episodically?',
    'Track the Supreme Court''s disposition of non-delegation and major-questions cases over the coming decade; count invalidations of statutory delegations and the formation (or absence) of a stable majority doctrine.',
    'If entrenched, the measured extraction and suppression realize fully and the constraint consolidates as a tangled rope with courts as durable agenda-setters; if repelled, the constraint decays toward theatrical invocation (piton symptoms) with rising theater_ratio and falling extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nondelegation_entrenchment_trajectory, empirical, 'Whether the revival visible at the interval endpoint consolidates or reverses.').

omega_variable(
    originalist_historical_record,
    'Does the founding-era record actually support impermeable boundaries — that the founding generation understood the vesting clauses to prohibit transfer of legislative power — given early practices (e.g., the 1790 Census Act, early collection statutes) that delegated discretionary tasks to executive officers?',
    'Systematic historiography of founding-era statutory practice and ratification debate, weighed against the formalist textual arguments; resolution lies in the professional historical literature rather than in advocacy scholarship from either camp.',
    'If the historical record shows the founders themselves accepted meaningful delegations, the reading''s lineage grounding weakens, its authority_erosion accelerates, and the constraint''s legitimacy claim shifts from inherited design to contemporary preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_historical_record, empirical, 'Whether the reading''s reference frame accurately reflects the founding practice it claims to restore.').

omega_variable(
    accountability_capacity_tradeoff,
    'How should the genuine coordination benefit (accountable lawmaking, anti-aggrandizement) be weighed against the destroyed regulatory capacity when the parties disagree about the weighting itself?',
    'Not resolvable by data alone: the formalist camp weights accountability lexicographically; the functionalist camp weights governance capacity heavily. Resolution requires an antecedent normative commitment about what the Constitution is for.',
    'Under an accountability-first weighting the constraint computes closer to rope (coordination dominant); under a capacity-first weighting it computes closer to snare (extraction dominant). The classification of this constraint is partly indexical to that value commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_capacity_tradeoff, preference, 'Irreducible value disagreement over the weighting of the constraint''s coordination benefit against its extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(sepa_tr_t0, observed).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__formalist_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(sepa_tr_t15, observed).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__formalist_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(sepa_tr_t30, observed).
narrative_ontology:measurement(sepa_tr_t45, separation_of_powers_text__formalist_reading, theater_ratio, 45, 0.58).
narrative_ontology:measurement_basis(sepa_tr_t45, observed).
narrative_ontology:measurement(sepa_tr_t60, separation_of_powers_text__formalist_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement_basis(sepa_tr_t60, observed).
narrative_ontology:measurement(sepa_tr_t75, separation_of_powers_text__formalist_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement_basis(sepa_tr_t75, observed).
narrative_ontology:measurement(sepa_tr_t90, separation_of_powers_text__formalist_reading, theater_ratio, 90, 0.28).
narrative_ontology:measurement_basis(sepa_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(sepa_be_t0, observed).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__formalist_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(sepa_be_t15, observed).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__formalist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(sepa_be_t30, observed).
narrative_ontology:measurement(sepa_be_t45, separation_of_powers_text__formalist_reading, base_extractiveness, 45, 0.36).
narrative_ontology:measurement_basis(sepa_be_t45, observed).
narrative_ontology:measurement(sepa_be_t60, separation_of_powers_text__formalist_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement_basis(sepa_be_t60, observed).
narrative_ontology:measurement(sepa_be_t75, separation_of_powers_text__formalist_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(sepa_be_t75, observed).
narrative_ontology:measurement(sepa_be_t90, separation_of_powers_text__formalist_reading, base_extractiveness, 90, 0.72).
narrative_ontology:measurement_basis(sepa_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(sepa_su_t0, observed).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__formalist_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(sepa_su_t15, observed).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__formalist_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(sepa_su_t30, observed).
narrative_ontology:measurement(sepa_su_t45, separation_of_powers_text__formalist_reading, suppression_requirement, 45, 0.38).
narrative_ontology:measurement_basis(sepa_su_t45, observed).
narrative_ontology:measurement(sepa_su_t60, separation_of_powers_text__formalist_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(sepa_su_t60, observed).
narrative_ontology:measurement(sepa_su_t75, separation_of_powers_text__formalist_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement_basis(sepa_su_t75, observed).
narrative_ontology:measurement(sepa_su_t90, separation_of_powers_text__formalist_reading, suppression_requirement, 90, 0.78).
narrative_ontology:measurement_basis(sepa_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, major_questions_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, chevron_deference_doctrine).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the 'separation of powers' label (epsilon-invariance principle): the colloquial concept covers at least three structurally distinct claims with different epsilon values, beneficiary sets, and victim sets. This story (formalist_reading) authors the non-delegation boundary rule: agencies are victims, regulatory capacity is the extracted resource, courts and challenge-financing industries collect. The functionalist_reading authors the flexibility framework over the same text: agencies sit near the beneficiary side, epsilon is low, and the victim set is nearly empty. The unitary_executive_reading authors executive-branch concentration: independent agencies are victims, but via presidential control rather than delegation invalidation. The formalist and functionalist readings cannot both govern (impermeable versus permeable boundaries); the formalist and unitary readings can cohold. Upstream/downstream: the formalist reading supplies the structural premise that major-questions-style doctrines radicalize, and its acceptance drains the interpretive foundation beneath chevron_deference_doctrine; edges declared accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
