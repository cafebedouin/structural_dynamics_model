% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of the Secession Legitimacy Boundary
 *   domain: political/federalism/constitutional
 *
 * SUMMARY:
 *   A provincial secession movement claims that a democratic majority within
 *   the provincial boundary holds ultimate sovereignty: a referendum yes is
 *   self-legitimating, federal authority is subordinate to it, and the
 *   movement's extraction claims against the federation are valid if the
 *   majority perceives them. The rule coordinates — it converts the least
 *   tractable question in federal politics into a terminal, countable
 *   decision procedure — while the same count transfers territory, resources,
 *   citizenship, and treaty relationships onto parties the procedure never
 *   counts. This file is ONE READING of the secession_legitimacy_boundary
 *   kernel; its ε is authored by this reading's own lights over the standing
 *   arrangement the rule constitutes: the reading genuinely registers the
 *   remedial function (a terminal exit from a grievance structure its
 *   majority could never outvote), so ε sits below what a hostile reading
 *   would score, but the unconsented transfer the rule performs on internal
 *   minorities and treaty nations is not denied even by the reading's own
 *   lights. The kernel's four readings are separate constraints with
 *   different ε: the constitutional impossibility reading seats legitimacy in
 *   the federal amendment process and protects the federal order (low ε from
 *   the federal seat); the grievance threshold reading seats legitimacy in
 *   federal injustice and its victim set is the aggrieved province; the
 *   treaty primacy reading seats legitimacy in Indigenous consent and its
 *   victim set is any secession performed without it. This reading's ε
 *   differs because its unconsented transfer falls precisely on the parties
 *   the treaty reading seats as gatekeepers and the impossibility reading
 *   protects — the family's disagreement is located in who must consent, and
 *   each reading prices a different non-consent.
 *
 * KEY AGENTS:
 *   - provincial_secession_leadership: agenda-setter (powerful/mobile) — sets the referendum question, declares the result, would enforce the exit; collects office and the transferred apparatus
 *   - provincial_secession_majority: primary beneficiary (organized/mobile) — the boundary-majority whose bare yes becomes sovereign statehood
 *   - provincial_internal_minorities: primary target (moderate/trapped) — bound to a state they rejected; no veto, no compensation, no exit
 *   - indigenous_treaty_nations: primary target (organized/trapped, civilizational horizon) — treaty counterparty changes without consent; sovereignty subordinated to the boundary the rule takes as given
 *   - remaining_federation_citizens: secondary target (moderate/constrained) — lose territory and compatriots by a vote they could not cast
 *   - federal_government: dual-positioned (institutional/trapped) — payer under this reading; agenda-setter and enforcer of the rival constitutional-impossibility reading
 *   - anti_secession_subregions: excluded (moderate/trapped) — internal majorities the rule's boundary denies the same plebiscitary force
 *   - constitutional_courts: analytical observer (institutional/analytical) — authored the rival reading's strongest institutional articulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.6).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.66).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/federalism/constitutional").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, 'fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e').
narrative_ontology:cs_kernel_codification('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', distributed).
narrative_ontology:cs_authority_grounding('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', self_enforcing).
narrative_ontology:cs_reading_relation('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', foundational, provincial_majority_plebiscite_self_legitimating).
narrative_ontology:cs_axiom_status(provincial_majority_plebiscite_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', provincial_majority_plebiscite_self_legitimating, deontological).
narrative_ontology:cs_axiom('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', secondary, majority_perception_validates_extraction_claims).
narrative_ontology:cs_axiom_status(majority_perception_validates_extraction_claims, holdable).
narrative_ontology:cs_axiom_grounding('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', majority_perception_validates_extraction_claims, conventional).
narrative_ontology:cs_reference_frame('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', provincial_plebiscitary_supremacy).
narrative_ontology:cs_drift_state('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', post_secession_reference_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fd2b3e5d-264a-4be6-9bc4-51afcfb5d19e', '2026-08-05T14:22:31Z').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secession_majority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secession_leadership).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_internal_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, remaining_federation_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, plebiscitary_democracy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, remedial_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The provincial government and the secessionist party's leadership. They choose whether, when, and with what wording to hold a referendum, campaign on the claim that a yes result settles the question, and would declare statehood and begin transferring jurisdiction on the strength of the count. A yes result puts them in charge of the new state's institutions, resource revenues, and negotiation posture. Their exit is the arrangement's product: they are the one group the rule hands a door.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secession_leadership, agenda_setter,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secession_leadership, beneficiary).

% Voters inside the provincial boundary who support secession — historically concentrated in a national-linguistic community that has never held the federation's decisive institutions. Under the rule, their bare majority converts into sovereign statehood without the consent of the federation, of internal minorities, or of treaty nations. What flows to them is a state whose borders, resources, and citizenship rules they would control; what they pay is the campaign and the uncertainty of the transition. Leaving is exactly what the rule exists to let them do.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secession_majority, beneficiary,
    organized, biographical, mobile, regional).

% Residents of the province who voted no or would bear the costs of exit without consenting: federalists, anglophone and immigrant communities, firms with cross-border integration. The rule binds them to a state they rejected; the procedure counts their votes only to lose. Moving out means abandoning homes, businesses, pensions, and community networks, and the rule provides no compensation mechanism and no veto. Their position inside the arrangement is the mirror image of the majority's: the same count that opens a door for one group closes the border behind another.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_internal_minorities, payer,
    moderate, biographical, trapped, regional).

% Nations holding treaties with the Crown, whose territories and peoples straddle the provincial boundary and predate it. The rule treats the provincial majority's will as supreme and counts treaty-nation consent among no required inputs; a yes result would change their treaty counterparty, split their peoples across a new international border, and subordinate their own sovereignty claims to a boundary drawn without them. They cannot exit the province, and the arrangement gives them no seat in the procedure that would redraw their world.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_nations, payer,
    organized, civilizational, trapped, regional).

% Citizens of the rest of the federation. They would lose territory, resource revenues, internal-market scale, and compatriots on the strength of a vote they had no ballot in; the rule gives federal institutions no veto and their consent no standing. Their recourse is negotiation leverage after a yes, not agreement before one. Exit is not meaningful — the federation is the thing being exited from.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, remaining_federation_citizens, payer,
    moderate, biographical, constrained, national).

% The federation's government and its constitutional apparatus. It administers and enforces the rival arrangement — the position that unilateral secession is impermissible and only negotiated amendment exit is legitimate — through courts, fiscal levers, and international recognition diplomacy. Under the rule this story instantiates, its consent is declared unnecessary and its authority subordinate to the provincial count. It cannot exit the dispute: its territory is the object. It bears the loss of jurisdiction while continuing to enforce the competing arrangement, which is why it holds a payer position inside this constraint and an agenda-setting position inside the rival one.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, agenda_setter).

% Majorities within particular cities and regions of the province that vote to remain — the rule grants self-legitimating force only to the majority at the provincial line, so their own majorities receive no equivalent standing. They are not seated in the procedure the rule defines: they can neither invoke it for themselves nor block its application to them. What they would say, from outside the conversation, is that the rule's decisive boundary is asserted rather than derived — and that the same majoritarian logic invoked to open the province's exit would, applied at their level, carve the province apart.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, anti_secession_subregions, excluded,
    moderate, biographical, trapped, local).

% The federation's highest courts. Their reference decision on secession held that a clear majority would oblige the federation to negotiate but would not confer a self-executing right to exit — the strongest institutional articulation of the rival reading, and the decision this reading's proponents refuse to accept as binding on the question. They take no side in the campaign; their seat is adjudicative, and under this reading's frame their role is subordinate to the plebiscite.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secession_majority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a terminal decision rule for the secession question: the majority within the provincial boundary decides, and the result requires no further validation. It converts an indefinitely contestable legitimacy dispute into a countable vote, giving secessionists, federal institutions, and international observers a single legible fact around which to coordinate.
% TRANSFER_FUNCTION: Moves sovereignty over territory, natural resources, citizenship, and the federal treaty relationship from the federation to the provincial majority's new state, on the strength of a provincial vote alone. The costs land on internal minorities who voted no, on treaty nations whose counterparty changes without consent, and on the rest of the federation — none of whom the rule's procedure counts.
% ABSENT_VOICES: Sub-provincial majorities that voted to remain: the rule fixes the decisive demos at the provincial boundary and gives their own majorities no self-legitimating force, so they stand outside the procedure it defines. Treaty nations are present in the wider dispute but absent from the rule's counting — their consent is not among its inputs. Both would object that the rule's decisive boundary is asserted, not derived.
% DISAPPEARANCE_RATIONALE: The secession movement's entire legitimacy strategy depends on this rule: without it, a yes result confers no self-executing exit and the question routes back through constitutional amendment, grievance adjudication, and treaty consent — the rival readings' procedures. Referendum timing, negotiation leverage, and internal-minority protections would all reorganize around those gates within a political generation.
% FOUNDING_PROBLEM: A provincial population formed into a federation without an ongoing consent mechanism found that every grievance channel — courts, amendment formulas, resource jurisdiction — ran through institutions its majority could never outvote. The rule was built to solve that terminal-exit problem: a standing, self-executing decision procedure no federal institution can veto, converting accumulated grievance into a single actionable vote.
% FOUNDING_PROBLEM_CORROBORATION: The consent deficit the rule targets is attested from outside the benefiting parties: constitutional scholars who reject the remedy nonetheless document the closed grievance structure, and the supreme court's own reference acknowledged that the federation's existing flexibility failed to contain the secessionist grievance. But no source outside the secessionist coalition attests the rule's specific framing unchanged — internal minorities and treaty nations attest that the real unconsented transfer is the one the rule itself performs. The founding problem is corroborated; the rule's solution to it is not.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.6 by this reading's own lights: the rule performs a real remedial function the reading recognizes — a terminal exit from a closed grievance structure — while imposing an unconsented transfer on internal minorities, treaty nations, and the federation; the reading justifies those costs but does not deny they are borne. Suppression (0.66) reflects what the rule's operation requires: overriding courts, binding a roughly 49 percent internal minority with no exit, and transferring treaty relationships without consent. Theater (0.35) is moderate: the referendum is a real counting mechanism, but the self-legitimation claim performs more mandate than a 50.x percent result carries — the gap between 'a narrow plurality opted for change' and 'the demos has spoken absolutely' is the performative share. Accessibility collapse (0.55): within the reading's frame the alternatives (amendment, adjudication, treaty consent) collapse entirely — the vote is held sufficient — but in the wider kernel contest all three remain live sibling readings, so collapse is incomplete in the world. Resistance (0.7) is high and structural: courts, the federal government, treaty nations, and internal minorities all actively contest the self-legitimation claim. All three metric series run on one shared nine-point grid. The cycle is documented, not noise: grievance accumulation, campaign, referendum peak, negotiation and stall, decay, rebuild. The oscillation is partly the mechanism itself — intermittent reinforcement: each cycle extracts uncertainty costs (capital flight, investment deferral, out-migration pressure) from internal minorities and the federation regardless of the final count. The base_properties scalars are measured at T=24, the rebuild phase on the rising limb of the second cycle, which is why the scalars sit slightly above the trough values rather than at the series maximum.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the secession majority and leadership seats, the rule is the remedy the federation never granted — the transfer reads as remediation, and the referendum is the first decision procedure they could ever win. From the internal-minority seat, the same rule is a transfer performed on them without consent: they bear the state they rejected, with exit priced at abandoning homes and community. From the treaty-nation seat, the rule subordinates their civilizational standing to a boundary drawn without them — what is taken is the assumption of jurisdiction itself. From the federal seat, the rule is dismemberment by plebiscite. The engine computes these divergences from the structural data (power, exit, role); the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the secession majority (directionality near the beneficiary end — the rule subsidizes its exit; its cost is the campaign) and the leadership (agenda-setter that also collects office and apparatus — near-beneficiary). Targets: internal minorities (trapped, no veto, no compensation — near full-target), treaty nations (trapped, civilizational stakes, consent uncounted — near full-target), remaining federation citizens (constrained exit, no ballot — high), and the federal government (loses jurisdiction and cannot exit its own territory — high, partially damped because it retains enforcement power over the rival reading). The excluded sub-provincial majorities sit near full-target on a rule they are barred from invoking. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place every seat correctly, because the rule's asymmetry is unusually clean — the counting procedure itself defines who benefits and who is counted only to lose.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. Mislabeling as pure snare would erase the genuine coordination function: the rule really does solve a collective-action problem — a terminal secession decision procedure — that the rival readings solve worse or not at all, and the secession majority is a real net beneficiary, not a cover story. Mislabeling as pure rope would erase the asymmetric extraction: the same counting procedure binds internal minorities and treaty nations who never consented and are given no exit, and the rule holds only through active enforcement against courts and federal authority. Mandatrophy is not in play: the founding problem (a closed grievance structure with no terminal exit) is live, corroborated by scholars who reject the remedy, so the constraint is not a degraded vestige. The live risk is drift, tracked in the measurement series: theater rises each cycle as the self-legitimation claim outruns the actual count, and if further cycles repeat without federal concession, the coordination share falls and the structure drifts toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the secession_legitimacy_boundary kernel; would instantiating a sibling reading — constitutional impossibility, grievance threshold, or treaty primacy — change the constraint''s beneficiary/victim structure and epsilon?',
    'Comparative classification of the four sibling stories: each reading''s victim set and epsilon are authored separately, and the engine''s cross-reading comparison locates the disagreement structurally in who must consent.',
    'If the treaty_primacy reading were adopted, indigenous_treaty_nations moves from unpriced payer to gatekeeping veto-holder and this reading''s epsilon rises (the unconsented transfer it performs becomes the corpus''s central case); if constitutional_impossibility were adopted, the provincial majority loses its exit and this rule''s coordination function collapses into procedural obstruction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the secession legitimacy kernel is instantiated and what sibling readings would change structurally.').

omega_variable(
    decisive_boundary_derivation,
    'Why is the provincial boundary the decisive demos? The rule asserts the inherited boundary as the self-legitimating unit rather than deriving it; sub-provincial majorities and Indigenous nations could claim the same plebiscitary force the rule grants only at the provincial line.',
    'A principled criterion for demos-formation — historical nationhood, treaty geography, or consent at each level — that either justifies the provincial boundary or relocates the decisive unit.',
    'If no criterion survives scrutiny, the rule''s transfer is revealed as boundary-arbitrary — the same majoritarian logic that liberates the provincial majority would dismember it at the city and nation level — and the structure drifts toward pure extraction; if a criterion survives, the rule''s coordination function is genuine and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decisive_boundary_derivation, conceptual, 'Whether the rule''s decisive boundary is principled or asserted.').

omega_variable(
    majority_perception_epistemics,
    'The rule validates extraction claims when the majority perceives them; does plebiscitary perception track actual federal-provincial transfer flows, or merely campaign-effective grievance?',
    'Independent fiscal and economic analysis of the federal-provincial transfer structure at referendum time, compared against the claims the winning majority ratified.',
    'If perception diverges systematically from measured flows, the rule''s validation standard is exploitable and its extraction index rises — the referendum launders contested grievance into legitimacy; if perception tracks the flows, the rule''s remedial function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_perception_epistemics, empirical, 'Whether plebiscitary perception tracks actual extraction.').

omega_variable(
    treaty_consent_durability,
    'Does the rule''s subordination of treaty-nation consent survive contact with entrenched Indigenous-rights constitutional protection and international norms requiring free, prior, and informed consent?',
    'Litigation and recognition practice: whether a unilateral secession declaration survives a treaty-nation challenge in domestic and international forums.',
    'If treaty consent hardens into an effective veto, this reading''s victim set gains a gate the rule cannot count, its coordination function fails across the treaty-territory share of the province, and the treaty_primacy sibling reading structurally supersedes this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_consent_durability, empirical, 'Durability of the rule''s subordination of treaty consent.').

omega_variable(
    referendum_threshold_ambiguity,
    'Is 50 percent plus one self-legitimating, or does the rule''s own democratic logic require a clear-majority threshold the rule as stated never fixes?',
    'The reading''s proponents must either defend the bare-majority threshold as principled or concede a clarity condition; the federal clarity statute and the court''s reference already force the question.',
    'A supermajority requirement shrinks the beneficiary set and strengthens internal-minority protection, lowering effective transfer force; bare majority maximizes the rule''s transfer force and its exposure to near-tie illegitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(referendum_threshold_ambiguity, preference, 'Where the rule''s own threshold sits — the ambiguity its self-legitimation claim papers over.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t3, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(sece_tr_t3, observed).
narrative_ontology:measurement(sece_tr_t6, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(sece_tr_t6, observed).
narrative_ontology:measurement(sece_tr_t9, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement_basis(sece_tr_t9, observed).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(sece_tr_t12, observed).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(sece_tr_t15, observed).
narrative_ontology:measurement(sece_tr_t18, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement_basis(sece_tr_t18, observed).
narrative_ontology:measurement(sece_tr_t21, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 21, 0.33).
narrative_ontology:measurement_basis(sece_tr_t21, observed).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(sece_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t3, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement_basis(sece_be_t3, observed).
narrative_ontology:measurement(sece_be_t6, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(sece_be_t6, observed).
narrative_ontology:measurement(sece_be_t9, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement_basis(sece_be_t9, observed).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(sece_be_t12, observed).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(sece_be_t15, observed).
narrative_ontology:measurement(sece_be_t18, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement_basis(sece_be_t18, observed).
narrative_ontology:measurement(sece_be_t21, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement_basis(sece_be_t21, observed).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(sece_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t3, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 3, 0.36).
narrative_ontology:measurement_basis(sece_su_t3, observed).
narrative_ontology:measurement(sece_su_t6, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(sece_su_t6, observed).
narrative_ontology:measurement(sece_su_t9, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement_basis(sece_su_t9, observed).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(sece_su_t12, observed).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(sece_su_t15, observed).
narrative_ontology:measurement(sece_su_t18, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(sece_su_t18, observed).
narrative_ontology:measurement(sece_su_t21, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement_basis(sece_su_t21, observed).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(sece_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'secession legitimacy' decomposes into four structurally distinct readings of one kernel, each with its own epsilon and victim set: this reading seats legitimacy in the provincial boundary-majority and leaves internal-minority and treaty consent unpriced; the constitutional impossibility reading seats it in the federal amendment process; the grievance threshold reading seats it in federal injustice; the treaty primacy reading seats it in Indigenous consent. Epsilon differs across the family because the unconsented transfer each reading performs falls on different parties. This story is the plebiscitary member; its referendum practice creates the legitimacy conditions the sibling readings respond to, and the court's reference decision (the impossibility reading's strongest articulation) is the principal downstream check on it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
