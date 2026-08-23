% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qi_accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine — Accountability-Void Reading (Impunity Transfer Regime)
 *   domain: constitutional law / civil rights / law enforcement policy
 *
 * SUMMARY:
 *   Since Pierson v. Ray (1967) and the crystallization at Harlow v.
 *   Fitzgerald (1982), federal courts have required that, before any damages
 *   proceeding begins, a plaintiff suing an officer for a constitutional
 *   violation show precedent 'clearly establishing' the right in question.
 *   The rule has no statutory text; it is judicially owned, uniformly applied
 *   nationwide, and actively maintained at every case. Its recorded
 *   operation: a large majority of civil-rights suits end at the immunity
 *   stage without discovery, trials against officers are rare, and the
 *   screening rationale coexists with grants in factually egregious cases.
 *   Interval mapping: calendar 1967–2023 onto t=0–56, one unit ≈ one year;
 *   grid points anchor to the Pierson origin, the Wood v. Strickland
 *   extension era, the Harlow crystallization, the clearly-established
 *   entrenchment of the late 1980s–early 1990s, the mandatory two-step era
 *   around 2000, the pre-Pearson dismissal peak, the sequence-first
 *   routine-dismissal era, and the persistence of the rule through the
 *   post-2020 reform-scrutiny period. This story authors the standing
 *   arrangement as it operates — who bears its costs, who is shielded, what
 *   closes, what resists.
 *
 * KEY AGENTS:
 *   - - federal_judiciary: agenda-setting administrator (institutional / analytical exit) — owns the rule, applies it case by case, absorbs the legitimacy cost of each grant
 *   - - officers_who_violate_constitutional_rights: shielded principals (organized / mobile) — the suit ends before their exposure begins
 *   - - liability_insulated_municipalities: dual-positioned fiscal beneficiary (institutional / constrained) — avoids judgments, retains residual indemnification and premium exposure
 *   - - police_union_leadership: collective defender (organized / mobile) — delivers the protection as a membership product
 *   - - victims_of_officer_constitutional_violations: primary cost-bearer (powerless / trapped) — remedy path closes before the merits open
 *   - - families_of_persons_killed_by_officers: bereaved cost-bearers (powerless / trapped) — years of motion practice ending without adjudication
 *   - - civil_rights_plaintiff_bar: unpaid risk-carrier (moderate / constrained) — finances the only accountability channel and absorbs its losses
 *   - - congressional_reform_majority: locked-out statutory authority (institutional / constrained) — passes bills that never reach enactment
 *   - - state_reform_legislatures: partial substituters (moderate / constrained, regional) — build parallel remedies that cannot reach federal claims
 *   - - empirical_accountability_scholars: analytical observers (analytical / analytical) — document what the rule actually does
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.85).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.78).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine — Accountability-Void Reading (Impunity Transfer Regime)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional law / civil rights / law enforcement policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '74174b74-9a3e-4ee6-959f-a9f1c09fac5e').
narrative_ontology:cs_kernel_codification('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', formalized).
narrative_ontology:cs_authority_grounding('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', lineage).
narrative_ontology:cs_interpretation_layer_present('74174b74-9a3e-4ee6-959f-a9f1c09fac5e').
narrative_ontology:cs_reading_relation('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', foundational, impunity_is_operative_function).
narrative_ontology:cs_axiom_status(impunity_is_operative_function, holdable).
narrative_ontology:cs_axiom_grounding('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', impunity_is_operative_function, empirically_contingent).
narrative_ontology:cs_axiom('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', secondary, remedy_nullification_transfers_value_to_shielded_officials).
narrative_ontology:cs_axiom_status(remedy_nullification_transfers_value_to_shielded_officials, holdable).
narrative_ontology:cs_axiom_grounding('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', remedy_nullification_transfers_value_to_shielded_officials, empirically_contingent).
narrative_ontology:cs_reference_frame('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', statutory_remedy_supremacy_baseline).
narrative_ontology:cs_drift_state('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', post_2020_reform_scrutiny, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('74174b74-9a3e-4ee6-959f-a9f1c09fac5e', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, officers_who_violate_constitutional_rights).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, liability_insulated_municipalities).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_union_leadership).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, victims_of_officer_constitutional_violations).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, families_of_persons_killed_by_officers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, liability_insulated_municipalities).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiff_bar).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, clearly_established_precedent_standard).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, official_immunity_common_law_inheritance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Creates and applies the rule case by case: every federal civil-rights suit against an officer passes through a judicial determination of whether existing precedent 'clearly establishes' the violated right, and every grant ends the case before discovery or trial. No statute defines the rule; the institution owns it entirely and maintains it through the precedent line, while individual judges occasionally dissent from its extensions. Each widely reported grant draws legitimacy criticism back onto the courts themselves.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Officers whose conduct is later judged unconstitutional — shootings, force deployments, warrantless entries — reach the liability stage only if a court first finds a materially similar precedent establishing the right. When the rule fires, the suit ends at summary judgment before their deposition, their personal assets are never exposed, and their employment record stays clean; union and municipal counsel run the precedent argument on their behalf. Changing departments or leaving policing altogether remains available.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, officers_who_violate_constitutional_rights, beneficiary,
    organized, biographical, mobile, national).

% Cities and counties avoid direct judgments and settlement payouts in the large fraction of claims dismissed at the immunity stage, and they size insurance reserves accordingly. Residual exposure persists in the claims that survive, in indemnification agreements covering officers, in premium trajectories, and in the political fallout after widely reported incidents. They cannot opt out of the federal rule; their levers are self-insurance, early settlement, and internal discipline.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, liability_insulated_municipalities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, liability_insulated_municipalities, payer).

% Union leaders bargain contracts and lobby legislatures on the presumption that members will not bear personal liability for on-duty constitutional errors, campaign publicly against reform bills, and spend member dues defending the rule. Delivering that protection is a core membership promise; losing it would be a representational defeat, though union leaders themselves could move to other roles in the labor ecosystem.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_union_leadership, beneficiary,
    organized, biographical, mobile, national).

% A person shot, beaten, unlawfully searched, or retaliated against files a federal civil-rights suit as the only formal accountability channel available. The suit typically ends at summary judgment when the court finds no sufficiently similar precedent 'clearly establishes' the right — before deposition, before discovery, before any adjudication of the underlying facts. Compensation, admission, and public vindication fail together, and there is nowhere else to file: the rule is uniform across the country.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, victims_of_officer_constitutional_violations, payer,
    powerless, biographical, trapped, national).

% Survivors pursue wrongful-death claims through years of motion practice that usually terminates at the immunity stage, carrying funeral and legal costs throughout. Occasional settlements arrive as negotiated exits without any adjudication of what happened, any finding against the officer, or any public record of fault.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, families_of_persons_killed_by_officers, payer,
    powerless, biographical, trapped, national).

% Contingency-fee lawyers advance these cases for years on speculation; when the rule fires, the investment writes down to zero and no fee ever arrives. The bar keeps filing because the federal suit remains the only vehicle that can produce a merits record, and moving to another practice area would mean abandoning the specialty they are credentialed in and known for.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiff_bar, payer,
    moderate, biographical, constrained, national).

% House majorities have repeatedly passed bills that would abolish or sharply narrow the doctrine; the Senate has never taken them up. Formally the body with statutory power over the remedy scheme, it currently sits outside the operative conversation, which runs through judicial precedent rather than legislation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, congressional_reform_majority, excluded,
    institutional, generational, constrained, national).

% Legislatures in a handful of states — Colorado in 2020, New Mexico in 2021 — enacted substitute causes of action that bypass officer immunity for state-law claims. Their statutes operate in parallel: they cannot reopen federal claims already dismissed, and their reach stops at state lines.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, state_reform_legislatures, excluded,
    moderate, generational, constrained, regional).

% Researchers assemble databases of federal civil-rights filings, code how often the immunity stage ends cases and on what rationales, and compare enforcement indicators before and after state-level repeals. Their findings circulate through journals, briefs, and hearings; they hold no vote in the doctrine's upkeep.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, empirical_accountability_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, officers_who_violate_constitutional_rights).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates the decision whether an official's conduct was 'clearly established' as unlawful into a federal-court precedent comparison that must conclude before any damages proceeding begins; pools litigation-risk assessment for officers and municipalities; and sequences discovery behind the immunity determination.
% TRANSFER_FUNCTION: Moves the enforceable value of constitutional claims — monetary compensation, merits adjudication, public vindication, and deterrent signaling — from injured individuals and their estates to shielded officers and to the municipal risk pools that would otherwise fund judgments and settlements.
% ABSENT_VOICES: The injured and the bereaved are structurally absent from the operative conversation: their cases end at summary judgment before their testimony matters, and prospective claimants deterred by the published dismissal rate never file at all. State legislators who built substitute remedies, and the senators whose chamber never acts, speak outside the forum where the rule is actually maintained.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, pending and previously dismissed suits would proceed to merits discovery; municipal insurers would reprice reserves and premiums; departments would recalibrate use-of-force policy against a live liability shadow; settlement volumes would spike while the courts and Congress competed to define whatever replaced the precedent-based screen. The accountability architecture would rearrange around whichever replacement won.
% FOUNDING_PROBLEM: Protect government officials performing discretionary duties from the harassment, distraction, and chilling cost of insubstantial civil-rights litigation, while nominally preserving exposure for the plainly incompetent or the knowingly lawless.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: peer-reviewed case-outcome research (Joanna Schwartz and collaborators) finds the doctrine rarely screens frivolous claims — most grants defeat factually serious suits, and frivolous filings are filtered by ordinary motions practice before the immunity stage; the Colorado (2020) and New Mexico (2021) repeals produced no measurable collapse in enforcement vigor or hiring; and sitting justices have dissented on record that the rule as applied bars relief for plainly unconstitutional conduct. No source outside the beneficiary set attests the founding problem as live and being served by this mechanism.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.85 because the standing arrangement under assessment — the doctrine as applied — closes the remedy for the large majority of claimants before discovery, with the bar set by a precedent-match test that even deliberate violations routinely pass. Suppression is authored at 0.78 as a RAW STRUCTURAL PROPERTY, unscaled by power or scope: the rule forecloses the federal civil remedy uniformly, repeated legislative overrides stall, and prospective filers are chilled by the published dismissal rate; only extractiveness gets scaled by the engine. Theater at 0.50 reflects that roughly half the doctrine's operative activity defends a screening function that outcome audits show rarely fires — at interval start the harassment-protection rationale was plausibly doing real work, and its share of performative maintenance grows as the impunity operation dominates. Accessibility collapse at 0.78: once a claimant understands the rule, alternatives (criminal referral, administrative discipline, state statutes, federal investigation) are nominal or unavailable, though not physically impossible — hence short of the near-total collapse of a genuine natural limit. Resistance at 0.65: sustained multi-front opposition (legislation, scholarship, state repeal, judicial dissent) that has not displaced the rule. The temporal series is a monotonic ratchet, not a cycle — each doctrinal step (objective standard, clearly-established requirement, mandatory sequencing, sequence-first dismissal) raised the enforcement effort needed to keep claims out while extraction accumulated; all three tracked metrics are authored on one shared eight-point grid, and the terminal values match the base-properties scalars. Receipt surface: gain_flow names the shielded officer because the proximate, reliable recipient of the rule's operation is personal immunity — municipal savings are real but derivative and contested (see the municipal-net-position omega). fixing_cost is 'cheap': courts created the rule without statutory authorization and can rescind it with ordinary votes, and states repealed analogues at ordinary legislative cost — measured against the benefit of restoring nationwide remedies, the mechanism's removal cost is low; what binds is coalition and will, which is resistance, not fix cost. Claim and metrics are independent authored facts: claimed_type snare states this reading's structural judgment (the coordination story is cover, persistence rides on active judicial enforcement and foreclosed exits, victims are identifiable); the metrics state what the arrangement descriptively does. The engine computes per-seat verdicts from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the shielded officer's position the rule is pure protection arriving at zero cost — subsidy-side through and through. From the trapped claimant's position it is the wall between injury and remedy, experienced at maximum target-side intensity with no exit to dampen it. The municipality sits genuinely dual: avoidance gains flowing in, indemnification, premiums, and backlash costs flowing out — the secondary payer role exists precisely so the engine prices both flows. The federal judiciary occupies a third position no beneficiary/victim declaration captures: it administers the mechanism, collects authority and docket control from administering it, and pays in legitimacy erosion with each notorious grant — near-symmetric, and left to the derivation chain's power-atom treatment rather than forced by an override. The plaintiff bar pays continuously yet stays engaged because its professional identity and the only merits-producing vehicle coincide. Equal-standing differentiation: congressional and state institutional actors hold formal power the rule simply ignores, because the rule's maintenance venue is the courtroom, not the capitol.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the shielded seats toward the beneficiary pole: officers with mobile exit sit nearest it; union leadership just above, paying dues and political capital for the protection it delivers; municipalities pulled back toward center by their secondary payer role (residual indemnification, premiums, reputational cost). Victim declarations with trapped exit drive the injured and the bereaved to the full-target end — no arbitrage exists anywhere in their choice set. The plaintiff bar derives a high-but-not-maximal target position: real uncompensated costs, but constrained rather than absent exit. The judiciary declares no beneficiary or victim relation; its directionality comes from the power-atom fallback, which lands it near the midpoint — the correct qualitative answer for an administrator who neither collects the shield nor bears the injury, but who does absorb the legitimacy bill, and the reason no override was authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — harassment of officials by insubstantial suits — is attested dead from outside the beneficiary set, while the arrangement persists and the world still rearranges around it; that status-by-verdict mismatch is exactly the zombie signal the R5 consumer is built to catch, and this story authors it openly rather than laundering the genealogy. Classifying the arrangement as this reading sees it prevents the protective cover story from being booked as coordination: a snare verdict forces the accounting to name who is coordinated, who pays, and what enforcement holds it up. The guard runs in both directions — the omega battery preserves the genuinely open empirical questions (whether repeal chills policing, whether municipalities net out positive) so the extraction account cannot quietly absorb trade-offs that might turn out to be real coordination costs. Mandatrophy here is not theatrical maintenance of a forgotten function but active maintenance of a profitable one; the theater metric tracks the fading protective rationale, not the vitality of the operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qi_kernel_reading_commitment,
    'This constraint is the accountability_void_reading of the qualified_immunity_doctrine kernel; which structural facts would change if a sibling reading were adopted as the operative classification?',
    'Cross-reading comparison within the kernel corpus: load a sibling''s file, diff its victim sets, epsilon, and claimed structure against this one, and let the engine''s per-kernel aggregation surface the divergence.',
    'Under the scaffold reading, victims of good-faith or negligent error would exit the victim set and epsilon would fall toward coordination-cost range; under the fidelity reading, the defect relocates from function to provenance and the extraction accounting becomes secondary. Neither change edits this file, which stands invariant on its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qi_kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what siblings would alter.').

omega_variable(
    deterrence_chill_natural_experiment,
    'Does removing the immunity bar measurably chill vigorous policing, hiring, or retention?',
    'State natural experiments: the Colorado (2020) and New Mexico (2021) repeals, compared with synthetic controls on use-of-force rates, clearance rates, hiring, and separations; supplemented by case-outcome audits of what the doctrine actually screens.',
    'If no chill materializes, the shield''s persistence buys no enforcement capacity and the near-target reading of the affected seats stands unopposed. If chill appears, part of what this reading books as extraction is the price of a genuine trade-off, and the arrangement acquires a real coordination component it currently denies having.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_chill_natural_experiment, empirical, 'The empirical wager beneath the protective rationale, resolvable by state repeal data.').

omega_variable(
    victim_coalition_formation_pathway,
    'Can isolated, individually powerless claimants aggregate — through crowdfunded dockets, advocacy networks, or class architectures — into organized resistance capable of shifting legislative or judicial outcomes?',
    'Track the funding and coordination infrastructure around federal civil-rights dockets after 2020; measure whether aggregated dockets survive the immunity stage at higher rates than isolated filings.',
    'If aggregation works, the resistance scalar understates durable opposition and persistence depends less on day-to-day closure of individual claims; if coordination structurally fails because cases die before networks compound, the closure mechanism''s role in persistence is confirmed and the powerless classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_formation_pathway, empirical, 'Whether the powerless seats can convert numbers into coalition power.').

omega_variable(
    municipal_net_position_ambiguity,
    'Are municipalities net gainers once insurance premiums, indemnification of officers, early settlements, and reputational costs are counted against the judgments they avoid?',
    'Municipal finance audits: reserve movements, premium trajectories, and settlement ledgers in high-incidence departments, set against measured dismissal-rate savings.',
    'Strongly net-positive municipalities widen the beneficiary structure and strengthen the multi-seat capture picture; roughly neutral ones narrow the capture set to officers and unions, concentrating the arrangement''s receipts and sharpening the accountability-void account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(municipal_net_position_ambiguity, empirical, 'Whether the municipal seat truly nets out as beneficiary or blurs into payer.').

omega_variable(
    theater_function_attribution,
    'Is the measured performative share the residue of a screening function that fails on the merits, or of a cover function succeeding at concealment?',
    'Code granted-immunity opinions against the underlying facts: how many grants rest on genuine close calls versus procedural posture — sequence-first denials issued before discovery, precedent-match failures on manufactured specificity.',
    'A failed-screen reading keeps a residual protective intent on the books and bounds the theater share below concealment levels; a concealment reading removes even that residual, raises the effective performative share, and hardens the impunity account of what the rule''s daily operation is for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_function_attribution, conceptual, 'What the growing performative share of doctrinal activity is actually doing.').

omega_variable(
    parallel_remedy_substitution_depth,
    'Do the parallel channels — new state statutes, administrative discipline, criminal referral, federal investigative patterns — constitute working substitutes for the nullified federal remedy, or nominal ones?',
    'Longitudinal counts: filings and outcomes under the Colorado and New Mexico statutes, sustained administrative discipline per incident class, criminal charges brought against officers, and opened federal pattern investigations, benchmarked against the volume of constitutionally adverse incidents.',
    'Working substitutes would lower the true accessibility-collapse score and soften the no-exit characterization of the trapped seats; nominal substitutes confirm the collapse measure and the trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_remedy_substitution_depth, empirical, 'Depth of the alternatives that remain after the federal remedy closes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_avoidance_void_tr_t0, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t0, observed).
narrative_ontology:measurement(qi_avoidance_void_tr_t8, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t8, observed).
narrative_ontology:measurement(qi_avoidance_void_tr_t16, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t16, observed).
narrative_ontology:measurement(qi_avoidance_void_tr_t24, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t24, observed).
narrative_ontology:measurement(qi_avoidance_void_tr_t32, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t32, observed).
narrative_ontology:measurement(qi_avoidance_void_tr_t40, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t40, observed).
narrative_ontology:measurement(qi_avoidance_void_tr_t48, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 48, 0.44).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t48, observed).
narrative_ontology:measurement(qi_avoidance_void_tr_t56, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 56, 0.5).
narrative_ontology:measurement_basis(qi_avoidance_void_tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(qi_avoidance_void_be_t0, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t0, observed).
narrative_ontology:measurement(qi_avoidance_void_be_t8, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t8, observed).
narrative_ontology:measurement(qi_avoidance_void_be_t16, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t16, observed).
narrative_ontology:measurement(qi_avoidance_void_be_t24, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t24, observed).
narrative_ontology:measurement(qi_avoidance_void_be_t32, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t32, observed).
narrative_ontology:measurement(qi_avoidance_void_be_t40, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t40, observed).
narrative_ontology:measurement(qi_avoidance_void_be_t48, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 48, 0.8).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t48, observed).
narrative_ontology:measurement(qi_avoidance_void_be_t56, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 56, 0.85).
narrative_ontology:measurement_basis(qi_avoidance_void_be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(qi_avoidance_void_su_t0, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t0, observed).
narrative_ontology:measurement(qi_avoidance_void_su_t8, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t8, observed).
narrative_ontology:measurement(qi_avoidance_void_su_t16, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t16, observed).
narrative_ontology:measurement(qi_avoidance_void_su_t24, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t24, observed).
narrative_ontology:measurement(qi_avoidance_void_su_t32, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t32, observed).
narrative_ontology:measurement(qi_avoidance_void_su_t40, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t40, observed).
narrative_ontology:measurement(qi_avoidance_void_su_t48, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t48, observed).
narrative_ontology:measurement(qi_avoidance_void_su_t56, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 56, 0.78).
narrative_ontology:measurement_basis(qi_avoidance_void_su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'qualified immunity' decomposes into three readings of one kernel, each a separate ε-invariant constraint. This file is the accountability_void_reading; its siblings (protective_scaffold_reading, constitutional_fidelity_reading) hold different victim sets, different epsilon assessments of the same standing arrangement, and different claimed structures. The readings are linked pairwise through affects_constraints so contamination and legitimacy pressure propagate across the family: documented impunity cases strengthen the fidelity critique's premises, and any empirical demonstration that repeal does not chill policing undermines the scaffold reading's load-bearing premise while leaving this file's internal accounting untouched. No single story hedges across readings; each stands on its own referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
