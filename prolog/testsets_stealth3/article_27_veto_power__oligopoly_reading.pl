% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: Article 27 P5 Veto — Geopolitical Oligopoly Reading
 *   domain: international relations/institutional design/constitutional law
 *
 * SUMMARY:
 *   Article 27(3) of the UN Charter grants the five permanent Security
 *   Council members unilateral power to defeat any substantive resolution,
 *   and Article 108 routes every Charter amendment through a ratification
 *   gate requiring each permanent member's consent. Read from the seat that
 *   assesses the arrangement as it operates rather than as it was justified
 *   at San Francisco, the structure concentrates binding collective-security
 *   authority in the five states positioned by the 1945 distribution of
 *   power, transfers the ongoing costs to the other 188 members, and
 *   reproduces itself indefinitely: the only legal path to revision runs
 *   through the parties whose position revision would end. Eighty years of
 *   membership growth (51 to 193), decolonization, and large shifts in
 *   economic and military capability have altered neither the gate nor its
 *   owners. The arrangement's public justification — that the veto keeps the
 *   great powers inside one tent and thereby prevents their war — is treated
 *   in this story as the arrangement's operating narrative, not as its
 *   measured function; the metrics record what the structure does, and the
 *   claimed type records what this reading holds it to be.
 *
 * KEY AGENTS:
 *   - - p5_permanent_members: Primary beneficiary and agenda-setter (institutional / arbitrage) — holds unilateral blocking power and the amendment gate; collects the arrangement's full authority premium; acts outside the framework whenever the framework would bind at a loss
 *   - - non_p5_member_states: Primary target (organized / trapped) — supplies universality, legitimacy, and financing; binding requests die at the gate; no exit preserves recognition and system access
 *   - - elected_council_members: Secondary target with incidental status benefit (moderate / constrained) — two-year seats, votes void on any substantive matter a permanent member opposes
 *   - - veto_blocked_populations: Excluded bearer of blocked-action consequences (powerless / trapped) — no seat, no vote, no proxy at the decision point
 *   - - sc_reform_coalitions: Excluded reform voice (organized / constrained) — continuous mobilization terminating at the ratification gate
 *   - - international_law_scholars: Analytical observer (analytical / analytical) — documents the structure and its drafting history; holds no lever over it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.82).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.76).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "Article 27 P5 Veto — Geopolitical Oligopoly Reading").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international relations/institutional design/constitutional law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'c87b96e0-b767-4003-9e49-8d84f49f8ec8').
narrative_ontology:cs_kernel_codification('c87b96e0-b767-4003-9e49-8d84f49f8ec8', fixed_text).
narrative_ontology:cs_authority_grounding('c87b96e0-b767-4003-9e49-8d84f49f8ec8', extraction).
narrative_ontology:cs_interpretation_layer_present('c87b96e0-b767-4003-9e49-8d84f49f8ec8').
narrative_ontology:cs_reading_relation('c87b96e0-b767-4003-9e49-8d84f49f8ec8', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('c87b96e0-b767-4003-9e49-8d84f49f8ec8', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c87b96e0-b767-4003-9e49-8d84f49f8ec8', foundational, authority_requires_representative_consent).
narrative_ontology:cs_axiom_status(authority_requires_representative_consent, holdable).
narrative_ontology:cs_axiom_grounding('c87b96e0-b767-4003-9e49-8d84f49f8ec8', authority_requires_representative_consent, deontological).
narrative_ontology:cs_axiom('c87b96e0-b767-4003-9e49-8d84f49f8ec8', foundational, self_protected_amendment_gate_is_privilege_capture).
narrative_ontology:cs_axiom_status(self_protected_amendment_gate_is_privilege_capture, holdable).
narrative_ontology:cs_axiom_grounding('c87b96e0-b767-4003-9e49-8d84f49f8ec8', self_protected_amendment_gate_is_privilege_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('c87b96e0-b767-4003-9e49-8d84f49f8ec8', victorious_coalition_privilege_settlement).
narrative_ontology:cs_drift_state('c87b96e0-b767-4003-9e49-8d84f49f8ec8', contemporary_multipolar_membership_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c87b96e0-b767-4003-9e49-8d84f49f8ec8', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, elected_council_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, elected_council_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states — China, France, Russia, the United Kingdom, the United States — hold unilateral power to defeat any substantive Security Council decision, and jointly hold the amendment gate: no Charter revision passes without each one's ratification. They rotate penholder control of draft resolutions, shield allies from Council action, and operate outside the UN framework whenever the framework inconveniences them, secure in the knowledge that no Council outcome can bind them against their will. Nothing in the arrangement costs them; maintaining it requires only the diplomatic capital spent deflecting reform proposals.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_permanent_members, beneficiary).

% The remaining 188 member states request, debate, and vote on binding collective-security action, but any substantive resolution dies if a single permanent member objects. Eight decades of membership growth, decolonization, and shifts in economic and military weight have changed whom the organization comprises without changing who decides. Withdrawal would forfeit recognition, treaty standing, and access to the system's financial and legal architecture, so no exit exists; proposing Charter revision leads to a gate the five control.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_member_states, payer,
    organized, generational, trapped, global).

% Ten states rotate onto the Council for two-year terms, gaining agenda visibility, diplomatic standing, and access to the permanent members' channels. Their votes are void on any substantive matter a permanent member opposes, and their draft resolutions survive only with permanent-member sponsorship. They leave the Council having lent their presence to decisions they could not shape.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, elected_council_members, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, elected_council_members, beneficiary).

% Civilian populations in conflicts where Council action is defeated — Syria after 2011, Ukraine after 2022, and others — live inside the consequences of collective responses that their situation argues for and a permanent member's interest defeats. They hold no seat, no vote, and no representative at the decision point; the blocked resolutions are the responses addressed to them.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, veto_blocked_populations, excluded,
    powerless, biographical, trapped, regional).

% Cross-regional groups pressing Council expansion and veto restraint — the G4 aspirant states, the Uniting-for-Consensus grouping, the ACT group's code-of-conduct signatories, the backers of the 2022 Liechtenstein initiative — organize proposals, assemble General Assembly majorities, and watch each initiative terminate at the ratification gate. Their mobilization is continuous; their access to the decision is nil.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, sc_reform_coalitions, excluded,
    organized, generational, constrained, global).

% Legal historians and institutional theorists trace the veto to the 1945 founding bargain and track its operation against the Charter's stated purposes. They publish critiques, advise delegations, and document the widening distance between the organization's universal membership and its five-member decision core; they hold no lever over the arrangement.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_law_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the five largest military powers inside a single collective-security institution by guaranteeing that no Council resolution can commit any of them to action it rejects — the entry price negotiated at San Francisco to secure a participation the League of Nations never obtained.
% TRANSFER_FUNCTION: Moves effective control over binding collective-security decisions from the membership at large (193 formally equal states) to five states holding unilateral blocking power; moves agenda-setting leverage the same way, since draft resolutions survive only within permanent-member tolerance; moves accountability away from the five, who cannot be bound by the organ they dominate.
% ABSENT_VOICES: Populations under veto-defeated conflicts have no seat and no proxy at the decision point. Roughly two-thirds of today's membership joined after 1955, never consented to the 1945 bargain, and had no vote at its creation. Small-state delegations whose drafts require permanent-member sponsorship speak only when permitted. The General Assembly hears all of them; the ratification gate does not.
% DISAPPEARANCE_RATIONALE: If the veto and its amendment gate vanished overnight, the Council's decision dynamics would reorganize immediately: the five would either accept majority-formed binding resolutions or defect visibly to ad hoc coalitions, draft-resolution sponsorship patterns would dissolve, and the reform coalitions' accumulated proposals would reach a decision point for the first time in eighty years. The organization's legitimacy economy — universal membership lending consent to a five-member decision core — would need rebuilding from one direction or the other.
% FOUNDING_PROBLEM: The League of Nations collapsed because its most powerful prospective members refused to join or defected, leaving collective security toothless. The 1945 founders designed the veto and the permanent-member ratification requirement as the entry price that would keep the United States, the Soviet Union, and the other great powers inside the organization permanently.
% FOUNDING_PROBLEM_CORROBORATION: The genealogy is externally corroborated: San Francisco Conference records and the drafting history of Articles 27 and 108 document the bargain as designed, with the 1920 US Senate rejection of the League covenant as the cited precedent. On status, attestation splits by seat: P5 governments attest the problem live, citing nuclear multipolarity and the requirement of great-power cohabitation; the G4, the ACT group's 129 code-of-conduct signatories, and a substantial body of international-relations scholarship attest that the operative function has shifted from participation-insurance toward privilege-maintenance. External corroboration exists for the genealogy and for the shifted-function hypothesis; no external party settles the contest.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.82 because the arrangement's yield — blocking power, agenda control, immunity from binding action — accrues entirely to five seats while the binding requests of 188 members terminate at a gate they cannot reach, and the gap widens mechanically as representativeness decays (membership 51 to 193; the non-P5 share of world product and armed force rose steeply across the interval). Suppression is 0.76 because the lock-in is structural and self-protecting: Article 108 requires each permanent member's ratification for any amendment, so the gate defends itself, and the suppression_requirement series shows the enforcement ratchet — coordinated blocking of reform initiatives intensified as pressure mounted (2005 World Summit failure, ACT code of conduct, 2022 Liechtenstein initiative met with P5 resistance). Theater is 0.48: nearly half of Council activity is performative — debates with predetermined outcomes, resolutions drafted to be defeated for signaling, symbolic Assembly substitutes — while real function (peacekeeping mandates, sanction regimes that clear the gate) persists alongside. Accessibility_collapse is 0.55: understanding the structure collapses the option of passing binding action against a permanent member's interest completely, but partial workarounds remain visible (Assembly emergency sessions, regional organizations, ad hoc coalitions, judicial routes) — degraded, not annihilated. Resistance is 0.62: continuous, organized, cross-regional, and structurally futile at the decision point. The theater series traces a cycle — Cold War paralysis, post-1991 revival, renewed paralysis — driven by great-power concord rather than by intermittent reinforcement; extraction rises monotonically beneath the cycle. Claim and metrics are independent authored facts: the snare claim comes from this reading's structural assessment, the metrics from the arrangement's observed operation; the engine computes per-seat classifications from the structural data without reference to either.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat the arrangement presents as sovereign prerogative and prudent design: no cost is borne, the gate reads as constitutional stability, and the cohabitation rationale reads as self-evidently sound. From the non-P5 seats the identical structure presents as disenfranchisement: votes voided, drafts killed, reform proposals terminated at a gate owned by the parties they would unseat. The elected members straddle the two: status benefit on arrival, nullification in operation. The engine computes these per-seat classifications from the structural data; this story's claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 sit at the beneficiary pole (d near 0): they collect the arrangement's entire yield and hold arbitrage-grade exit — they act outside the Charter whenever it suits them, so the arrangement never binds them at a loss. The non-P5 majority sits at the target pole (d near 1): they supply the organization's universality, legitimacy, and financing while their binding requests die at the gate; exit is trapped because UN membership is the sole source of universal legal recognition, and leaving forfeits treaty standing, recognition, and access to the system's financial and legal architecture. Elected members derive a mixed position — incidental status benefit against structural nullification. Blocked populations are full targets with zero positional power. The derivation chain reads these positions from the beneficiary/victim declarations and exit atoms; no directionality overrides were needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The veto's founding mandate — keeping the great powers inside one collective-security tent after the League's collapse — is among the most effective cover stories institutional extraction has ever operated behind, because the good it invokes is real and enormous. The classification machinery prevents two opposite mislabels. Reading the arrangement as pure coordination would require broad beneficiaries and extraction near the coordination floor; instead beneficiaries concentrate in five seats while costs spread across 188 — the asymmetry signature that separates hybrid and extractive forms from genuine coordination. Reading it as inertial residue would fit a constraint nobody maintains; this one is actively maintained — vetoes cast, reform initiatives blocked, interpretive practices adjusted — so inertia is not the persistence mechanism. The R5 interview records the founding problem as contested: the P5 attest the cohabitation problem live; the reform blocs and much of the scholarship attest functional conversion. The snare claim follows from the reading's structural facts, not from resolving that contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_attribution,
    'This story instantiates the oligopoly_reading of kernel article_27_veto_power. Which of the three declared readings — coordination, oligopoly, sovereignty — correctly attributes the veto''s operative function?',
    'Comparative classification across the three sibling stories: if the coordination reading computes near-floor extraction with a broad beneficiary set while this reading computes a concentrated beneficiary with trapped targets, the disagreement localizes to function attribution rather than to any measurable quantity of the mechanism itself.',
    'Resolution determines whether the veto enters the corpus as a coordination cost priced into great-power cohabitation or as enforced extraction from the non-permanent majority; the sibling files carry the same referent with reading-indexed epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_attribution, conceptual, 'Which reading of the Article 27 kernel captures the veto''s operative function.').

omega_variable(
    sibling_structural_delta,
    'What would adopting a sibling reading change structurally? The sovereignty_reading relocates the victim set (framing the veto as each great power''s consent-right recasts non-P5 states as free riders on withheld consent rather than as targets), and the coordination_reading relocates the beneficiary set (humanity-at-large as beneficiary of great-power cohabitation). Where exactly is the disagreement located?',
    'Locate the disagreement element: all three readings agree on the mechanism (unilateral blocking power, P5-only amendment gate) and diverge on what the mechanism is for — adjudicate by observing whose interests the arrangement has actually served across eighty years of veto-use history.',
    'If the disagreement reduces to beneficiary/victim attribution, the three stories differ only in directionality mappings over one mechanism; if it reduces to the legitimacy of the amendment gate itself, the sovereignty reading diverges structurally rather than indexically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Structural deltas between this reading and its sibling readings.').

omega_variable(
    deterrence_vs_veto_confound,
    'Did the veto prevent great-power war, or did nuclear deterrence? The arrangement''s operating narrative rests on a causal claim that cannot be isolated from the nuclear revolution that coincided with the institution''s founding.',
    'Quasi-natural experiments: compare crises where collective security operated without an available veto (Korea 1950 via General Assembly Resolution 377(V), Suez 1956 Assembly-forced withdrawal, Congo 1960) against crises where the veto blocked action outright (Syria after 2011, Ukraine after 2022), tracking escalation outcomes.',
    'If great-power peace holds in the no-veto cases, the war-prevention justification loses its evidentiary base and this reading''s account of the arrangement as privilege-maintenance strengthens; if the no-veto cases show elevated escalation risk, part of the measured extraction is re-priced as genuine insurance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_vs_veto_confound, empirical, 'Whether war-prevention credit belongs to the veto mechanism or to nuclear deterrence.').

omega_variable(
    reform_path_existence,
    'Does any feasible reform or exit pathway exist that the structural data treats as closed — extra-Charter practice evolution, treaty-based parallel institutions, or mass-membership defection?',
    'Track cumulative General Assembly practice (Uniting for Peace invocations, the 2022 veto-initiative''s automatic follow-up debates, budget-withholding episodes) for emergent substitution that lowers the effective cost of routing around the Council.',
    'If substitution matures, measured suppression falls and the arrangement''s classification drifts toward hybrid forms; if the five suppress substitution itself through budget and staffing leverage, suppression is confirmed higher than the scalar suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_path_existence, empirical, 'Whether a viable reform or exit path exists outside the P5-controlled amendment gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__oligopoly_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(arti_tr_t1955, article_27_veto_power__oligopoly_reading, theater_ratio, 1955, 0.25).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__oligopoly_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(arti_tr_t1980, article_27_veto_power__oligopoly_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(arti_tr_t1995, article_27_veto_power__oligopoly_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(arti_tr_t2010, article_27_veto_power__oligopoly_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__oligopoly_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(arti_be_t1955, article_27_veto_power__oligopoly_reading, base_extractiveness, 1955, 0.58).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__oligopoly_reading, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement(arti_be_t1980, article_27_veto_power__oligopoly_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(arti_be_t1995, article_27_veto_power__oligopoly_reading, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement(arti_be_t2010, article_27_veto_power__oligopoly_reading, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__oligopoly_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(arti_su_t1955, article_27_veto_power__oligopoly_reading, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__oligopoly_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(arti_su_t1980, article_27_veto_power__oligopoly_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(arti_su_t1995, article_27_veto_power__oligopoly_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(arti_su_t2010, article_27_veto_power__oligopoly_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(arti_su_t2025, article_27_veto_power__oligopoly_reading, suppression_requirement, 2025, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint-family note for kernel article_27_veto_power: the colloquial label 'the P5 veto' covers three structurally distinct claims instantiated as three linked stories sharing one mechanism. This file authors the oligopoly instantiation (epsilon 0.82; concentrated beneficiary; trapped majority). The coordination instantiation prices the same mechanism as great-power cohabitation insurance with a broad beneficiary set and near-floor extraction; the sovereignty instantiation reframes it as consent-rights at great-power scale with the victim set relocated to would-be subjects of non-consented binding law. Edges run to both siblings for contamination tracing; no upstream/downstream ordering is asserted because the readings share one mechanism and diverge only in function attribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
