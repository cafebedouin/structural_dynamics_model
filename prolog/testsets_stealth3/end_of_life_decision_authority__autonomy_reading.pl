% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: End-of-Life Decision Authority: Standing Medico-Legal Gatekeeping Regime (Autonomy Reading)
 *   domain: bioethics/end-of-life policy
 *
 * SUMMARY:
 *   This story instantiates the autonomy_reading of the
 *   end_of_life_decision_authority kernel: the claim that competent
 *   individuals possess sovereign authority over their own death. Per the
 *   kernel-reading epsilon-referent rule, extractiveness is authored for the
 *   standing arrangement under contest — the medico-legal regime that
 *   withholds final end-of-life decision authority from competent individuals
 *   and vests it in criminal prohibition, medical gatekeeping, and
 *   institutional review — assessed by THIS reading's own lights, never by
 *   the reading's endorsed alternative. By this reading's lights the standing
 *   regime is substantially extractive: it prices the protection of dependent
 *   patients in the currency of prolonged suffering charged to a competent
 *   class that does not need or want that protection, and it enforces the
 *   price with criminal law and licensing discipline. The expected structural
 *   delta for this reading is realized in the structural data: the
 *   suffering-prolonged denied access enter the victim set; healthcare
 *   professionals appear in dual position as the regime's enforcement front
 *   line and its moral-cost bearers; and slippery-slope risk is externalized
 *   — coercion-of-the-vulnerable concerns are seated in the
 *   vulnerability_protection sibling's story, not here. The sibling readings
 *   are separate constraints with separate, reading-indexed epsilon values
 *   over the same shared referent; this file links them via
 *   network.affects_constraints. Claim and metrics are independently
 *   authored: the claimed type is this reading's structural assessment of the
 *   regime, and the metrics are its descriptive assessment — neither was
 *   tuned toward the other or toward a predicted engine output.
 *
 * KEY AGENTS:
 *   - competent_suffering_individuals: primary target (powerless/trapped) — bears the regime's cost as prolonged living against settled will, with access denied
 *   - vulnerable_dependent_patients: coordinated protected class (powerless/trapped) — shielded from pressure to die by the same structure that prices the shield on others
 *   - medical_professional_bodies: beneficiary (institutional/constrained) — retains gatekeeping jurisdiction over death; absorbs drift through guidance and ethics practice
 *   - religious_institutions: beneficiary (organized/identity_locked) — collects doctrinal vindication from prohibition; cannot reposition without doctrinal dissolution
 *   - bedside_physicians: dual-positioned enforcer (organized/constrained; agenda_setter with secondary payer position) — executes gatekeeping at the point of request while bearing moral distress and prosecution exposure
 *   - legislative_judicial_authorities: agenda_setter (institutional/arbitrage) — redraws the authority map and can exit by rewriting it
 *   - prosecuted_compassionate_assisters: secondary target (powerless/trapped) — family members and clinicians bearing the enforcement machinery's cost
 *   - cross_border_dying_travelers: partial-exit target (moderate/constrained) — buys partial relief through foreign jurisdictions, rationed by money and medical eligibility
 *   - nonterminal_suffering_excluded: excluded voice (powerless/trapped) — chronic, degenerative, and psychiatric suffering carved out of even reform statutes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.68).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.62).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "End-of-Life Decision Authority: Standing Medico-Legal Gatekeeping Regime (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '75eaa64e-606b-4f06-8d72-e636bd8ccd02').
narrative_ontology:cs_kernel_codification('75eaa64e-606b-4f06-8d72-e636bd8ccd02', distributed).
narrative_ontology:cs_authority_grounding('75eaa64e-606b-4f06-8d72-e636bd8ccd02', extraction).
narrative_ontology:cs_interpretation_layer_present('75eaa64e-606b-4f06-8d72-e636bd8ccd02').
narrative_ontology:cs_reading_relation('75eaa64e-606b-4f06-8d72-e636bd8ccd02', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('75eaa64e-606b-4f06-8d72-e636bd8ccd02', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('75eaa64e-606b-4f06-8d72-e636bd8ccd02', foundational, competent_will_sufficient_authority).
narrative_ontology:cs_axiom_status(competent_will_sufficient_authority, holdable).
narrative_ontology:cs_axiom_grounding('75eaa64e-606b-4f06-8d72-e636bd8ccd02', competent_will_sufficient_authority, deontological).
narrative_ontology:cs_axiom('75eaa64e-606b-4f06-8d72-e636bd8ccd02', secondary, denied_self_determination_is_the_primary_harm).
narrative_ontology:cs_axiom_status(denied_self_determination_is_the_primary_harm, holdable).
narrative_ontology:cs_axiom_grounding('75eaa64e-606b-4f06-8d72-e636bd8ccd02', denied_self_determination_is_the_primary_harm, deontological).
narrative_ontology:cs_reference_frame('75eaa64e-606b-4f06-8d72-e636bd8ccd02', individual_sovereign_self_determination).
narrative_ontology:cs_drift_state('75eaa64e-606b-4f06-8d72-e636bd8ccd02', contemporary_legalization_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('75eaa64e-606b-4f06-8d72-e636bd8ccd02', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, vulnerable_dependent_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, medical_professional_bodies).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, religious_institutions).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, competent_suffering_individuals).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, prosecuted_compassionate_assisters).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, cross_border_dying_travelers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, bedside_physicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, medical_gatekeeping_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Terminally ill or unbearably suffering adults with decision-making capacity whose repeated, settled requests to end their lives are refused by law and by medical gatekeeping. What flows to them is continued living under conditions they have rejected; what flows from them is compliance enforced by criminal law. Their exits: enduring, violent self-endangerment that risks worse injury, or travel to a permissive jurisdiction that costs more than most dying people have and is often medically closed to them by the time they need it.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_suffering_individuals, payer,
    powerless, immediate, trapped, national).

% Elderly, disabled, and dependent patients whose families or caregivers might benefit from their deaths. The gatekeeping apparatus stands between them and any pressure to die: requests are scrutinized, capacity is examined, and no life-ending act proceeds on mere say-so. The protection costs them nothing directly; its price is carried by the class of competent sufferers described above.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, vulnerable_dependent_patients, beneficiary,
    powerless, biographical, trapped, national).

% Licensing colleges and professional associations that hold final gatekeeping authority over end-of-life decisions. The arrangement preserves a jurisdiction they have exercised for a century: no death is chosen without passing through their members' assessment. They absorb change through guidance documents and ethics-committee practice rather than statutory revision, and several have shifted from outright opposition to studied neutrality as reform waves advanced.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, medical_professional_bodies, beneficiary,
    institutional, generational, constrained, national).

% Churches and doctrinal bodies whose teaching holds that life's value does not depend on the individual's will. The standing arrangement embeds that teaching in law, and its persistence vindicates their authority in the public square across every jurisdiction they operate in. Their position is not revisable without doctrinal dissolution: repositioning on intentional death would unravel commitments that constitute the institution itself.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_institutions, beneficiary,
    organized, generational, identity_locked, global).

% The clinicians at the point of request. They assess capacity, delay, refuse, and in prohibition jurisdictions face criminal exposure and licensing ruin if they assist compassionately; many carry the moral cost of watching patients die violently or travel abroad to die among strangers. Where reform has come, they administer the new statutes' conditions — becoming licensed facilitators under rules they did not write. Their exit is bounded: they can leave end-of-life practice but not the arrangement that governs it.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bedside_physicians, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, bedside_physicians, payer).

% Legislatures and courts that set and periodically redraw the authority map — absolute prohibition, prosecutorial discretion, licensed access with safeguards. They can revisit the arrangement at will and several have; their relationship to it is one of authorship, not subjection.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislative_judicial_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Spouses, adult children, friends, and clinicians who helped a competent person die and were investigated, prosecuted, or disciplined for it. They acted at the dying person's settled request and bear the enforcement machinery's cost: trials, convictions, professional ruin. Their exit is nothing — the act is done before the law arrives.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, prosecuted_compassionate_assisters, payer,
    powerless, immediate, trapped, national).

% Dying people with enough money and enough medical eligibility to reach foreign right-to-die organizations. They buy partial relief from the arrangement — a legal death, abroad, among strangers — at the price of travel their bodies may not tolerate, costs their families carry, and a legal gray zone for anyone who accompanies them. Money and timing ration the exit.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, cross_border_dying_travelers, payer,
    moderate, immediate, constrained, continental).

% People suffering unbearably from chronic, degenerative, or psychiatric conditions that no reform statute covers: every assisted-dying law on the books requires terminal prognosis or excludes mental suffering outright. They would object to being carved out of even the reformed arrangements and are structurally absent from the drafting tables; their requests are refused without a hearing.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, nonterminal_suffering_excluded, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, medical_professional_bodies).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solves a real collective problem: it prevents coerced, mistaken, and non-voluntary deaths of dependent people by requiring institutional verification before any life-ending act, and it maintains a single medical-legal protocol for end-of-life decisions across millions of deaths.
% TRANSFER_FUNCTION: Moves final decision authority over death from competent individuals to medical-legal gatekeeping institutions; moves the price of that protection to the competent suffering class, paid in prolonged living against settled will; moves enforcement risk onto family members and clinicians who assist.
% ABSENT_VOICES: The non-terminal suffering — chronic, degenerative, psychiatric — are excluded from every reform statute's scope and from the drafting tables that wrote them. The dead under the arrangement (violent suicides, the deceased of prosecuted assisters, those who died abroad among strangers) cannot testify; their objection survives only as statistics and second-hand accounts. Both groups would object that the conversation's boundaries were drawn without them.
% DISAPPEARANCE_RATIONALE: If the gatekeeping arrangement vanished overnight, end-of-life decisions would reorganize around individual-plus-physician determination within existing medical relationships; medical bodies would lose a century-old jurisdiction they currently absorb drift through; dependent-patient protections would need deliberate replacement rather than existing by default; the dying would reorganize final decisions around new defaults; and the prosecution risk currently shadowing compassionate assistance would evaporate.
% FOUNDING_PROBLEM: The arrangement was consolidated to prevent the recurrence of non-voluntary killing and coerced death: the eugenics-era record of involuntary euthanasia, and medicine's history of unilateral life-ending decisions, were to be answered by placing end-of-life decisions under institutional control rather than individual or familial will.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: disability-rights organizations opposing assisted dying attest that pressure on dependent patients is real and present; elder-abuse and safeguarding literature documents inheritance-motivated pressure; prosecutorial case records show coerced-death attempts. The vulnerability_protection sibling reading exists precisely because this problem is live. Note that the autonomy reading itself corroborates the problem's liveness while disputing that the standing arrangement is the right instrument for it — corroboration of the problem is not corroboration of the regime.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (end-state) by this reading's lights: the regime's core operation is requiring competent individuals to continue living against their settled, repeated will, with prosecution risk attached to anyone who helps them stop. Suppression (0.62) is a raw structural property — criminal prohibition, licensing discipline, travel friction — and is NOT scaled by directionality or scope; only extractiveness is engine-scaled by d and spatial scope. Theater (0.45) rises across the interval: as reform pressure mounted, prohibition jurisdictions increasingly governed through prosecutorial-discretion policies that never enforce but never reform, and through ethics review that delays without deciding — performative maintenance of a prohibition no longer honestly enforced (Goodhart drift of the protective proxy). Accessibility collapse is moderate (0.55): exits exist — foreign jurisdictions, voluntary stopping of eating and drinking, terminal sedation, underground assistance — but are rationed by money, medical eligibility, and legal gray zones, so alternatives narrow without closing. Resistance is high (0.6): constitutional litigation, sustained advocacy, clinician civil disobedience, and jurisdiction-by-jurisdiction reform waves meet the regime continuously. All three tracked metric series run on one shared time grid (1976/1986/1996/2006/2016/2026) with every metric authored at every point; the trajectories are monotonic rather than cyclical — extractiveness and suppression decay as legalization spreads while theater rises as enforcement goes symbolic. The flattening extractiveness curve encodes this reading's specific complaint: even reform statutes replace prohibition with gatekeeping rather than sovereignty, putting a floor under epsilon that no further reform wave in this interval removed.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda seats should compute divergent types from the same structure. From competent_suffering_individuals (powerless, trapped) the regime computes as near-total denial with a protective story they cannot opt out of; from medical_professional_bodies (institutional, constrained) the same structure computes as professional duty, liability management, and a jurisdiction held for a century; from legislative_judicial_authorities (institutional, arbitrage) it is settled law they revisit at will. Inter-institutional dynamics: medical bodies and religious institutions experience reform waves differently — the bodies can reposition (several have moved from opposition to neutrality) because their stake is jurisdictional, while religious institutions cannot reposition without doctrinal dissolution, hence the identity_locked exit; their identity fusion is doctrinal (the institution has become its teaching on intentional death), and if that frame broke, their seat would recompute as ordinary organized opposition. Same-level lateral dynamics: within the victim class, competent_suffering_individuals (trapped) and cross_border_dying_travelers (constrained, near-arbitrage) hold the same formal position with different exits — money and medical timing buy partial exit, differentiating their effective position despite identical status. Coalition dynamics cap themselves structurally: the victim class has organized advocacy, but its members die on a schedule the constraint itself sets, so class power is permanently time-limited in a way no other victim class in this corpus is. Bedside physicians carry a professional-identity fusion of their own — the healer norm makes assistance unthinkable for some and conscience-objection mandatory for others — which differentiates physicians within the same seat. Suppression here is predominantly structural (criminal law, licensing); the internalized components (internalized healer norms, internalized duty-to-die pressure) are seated in the sibling stories per this reading's externalization.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive d. vulnerable_dependent_patients sit near the beneficiary end: the arrangement subsidizes them with protection at zero direct price. medical_professional_bodies and religious_institutions collect authority and vindication respectively. legislative_judicial_authorities hold agenda control with arbitrage exit, placing them near the beneficiary end despite writing the rules — they can leave the arrangement's costs behind by amending it, and several have. The victim class sits near the target end, differentiated by exit: trapped competent_suffering_individuals and prosecuted_compassionate_assisters take near-full-target d; the purchased partial exit of cross_border_dying_travelers damps theirs. bedside_physicians are genuinely dual-positioned — they administer the gatekeeping while bearing its moral and legal costs — so the secondary_role payer declaration exists precisely so the engine computes a mid-range d rather than reading them as pure enforcers; the expected structural delta (professionals as facilitators) describes the reading's endorsed arrangement and appears here as the reform-jurisdiction half of their situation. Spatial scope is national for most seats (the regime is jurisdictional), global for religious institutions operating across jurisdictions, continental for travel-based exit. Larger scope amplifies effective extraction modestly in the engine's computation; the national scope of the primary target seat keeps that amplification bounded.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabelings. Reading the regime as a mountain (natural law: society must never permit intentional death) fails: emerges_naturally is false, the arrangement is enacted statute and case law, and it was rewritten jurisdiction-by-jurisdiction across the interval — the declining extractiveness and suppression series record a constructed constraint under active revision, not an irreducible limit. Reading it as pure extraction with a cover story also fails: the coordination function is genuine — dependent patients are shielded from pressure by institutional verification, and the founding problem (coerced and non-voluntary death) is corroborated live by parties outside the beneficiary set. Tangled_rope holds both facts: real coordination for one class, asymmetric incidence — protection for one class priced in the suffering of another through the same structure, held by active enforcement. Mandatrophy is not resolved: the founding mandate remains live (founding_problem_status live, corroborated), so the arrangement has not outlived its function; this reading's complaint is over-service at asymmetric cost, not obsolescence. The R5 mismatch check (status live x disappearance verdict world_rearranges) accordingly raises no zombie flag. The receipt surface records where the gains land: gatekeeping authority accrues demonstrably to medical_professional_bodies, and fixing_cost is classed cheap — not because reform is costless, but because it is procedurally proven in a dozen comparable jurisdictions, making the barrier political coordination rather than feasibility; the captured seat keeps the arrangement snare-flavored under either cost class regardless of this story's claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_reading_kernel_membership,
    'This constraint is one reading of the end_of_life_decision_authority kernel (the autonomy_reading); what would the sibling readings change structurally if instantiated instead?',
    'Instantiate the sibling stories: the sanctity_reading would empty this victim set (no denied-access class exists where prohibition is the point) and seat healthcare professionals as refusers; the vulnerability_protection_reading would split the victim set into the denied and the coerced and seat the institutional checkpoints themselves as the constraint.',
    'Classification is reading-indexed: the same referent computes as tangled_rope from this seat, but would compute differently from the sanctity seat (low reading-indexed extraction, mountain-claimed) and the vulnerability_protection seat (moderate, harms in both directions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_reading_kernel_membership, conceptual, 'Committer structure: which kernel, which reading, what the sibling readings would change.').

omega_variable(
    disagreement_location_final_authority,
    'Where exactly do the three readings disagree, and which structural element moves when the disagreement is resolved?',
    'Locate the disagreement in the locus of final authority over death (individual will vs intrinsic value independent of will vs institutional distribution); test by asking each reading who may say no to a settled, repeated, competent request to die.',
    'Resolving the locus reassigns the entire victim set: will as final authority seats the suffering-prolonged; intrinsic value seats no denied-access class; distribution splits the class between the denied and the coerced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_final_authority, conceptual, 'The kernel contest is a dispute over the seat of final authority, not over empirical facts.').

omega_variable(
    coercion_prevention_efficacy,
    'Does institutional gatekeeping actually reduce coerced or non-voluntary death relative to permissive regimes with safeguards — the premise of the arrangement''s protective coordination function?',
    'Comparative jurisdictional data: coerced-death incidence, elder-abuse and pressure indicators, and safeguard outcomes across prohibition, licensed-access, and permissive regimes.',
    'If gatekeeping does not reduce coercion incidence, the coordination function thins and the arrangement computes toward pure extraction from more seats; if it does, part of the measured extraction is the price of protection rather than overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_prevention_efficacy, empirical, 'Whether the protective coordination function is genuine or cover.').

omega_variable(
    competence_boundary_contestation,
    'The victim set is bounded by competence determinations the contested regime itself administers — does the competent/incompetent line hold at the boundaries (depression, disability, dementia, fluctuating capacity)?',
    'Capacity-assessment outcome data at the boundary cases, plus the rate at which competence findings reverse under appeal or re-examination.',
    'If the competence line systematically excludes people with settled wills, the victim set is larger than authored and this reading understates extraction; if it holds, the reading''s clean victim class is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_boundary_contestation, conceptual, 'This reading''s victim class depends on a boundary drawn by the regime it contests.').

omega_variable(
    externalized_slippery_slope_risk,
    'This reading externalizes coercion-risk (slippery slope) to the vulnerability_protection sibling''s story — what happens to this story''s beneficiary structure if coercion pressure emerges endogenously inside autonomy-style regimes?',
    'Longitudinal data from long-standing permissive jurisdictions on pressure on elderly, disabled, and dependent patients, including criteria-expansion trajectories over time.',
    'If the risk internalizes, vulnerable_dependent_patients stop being pure beneficiaries in this story too, and the arrangement''s coordination/extraction balance shifts — the tangled_rope claim weakens toward pure extraction from this reading''s own seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_slippery_slope_risk, empirical, 'Whether this reading''s externalization of slippery-slope risk is structurally stable.').

omega_variable(
    cs_authority_framing_underdetermination,
    'authority_grounding is authored as extraction (institutions benefit from kernel non-revision, with an interpretive layer absorbing drift); is the alternative framing — distributed legal authority with no single extractor — the better commitment-system classification?',
    'Test whether any single institution''s benefit from non-revision is load-bearing: if medical bodies, religious institutions, and courts each hold only partial stakes in kernel stability, the distributed framing fits better.',
    'Under the distributed framing the CS pattern changes (no designated interpreter; drift reads as jurisdictional variance rather than absorbed drift) while the constraint''s metric classification is unaffected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_underdetermination, conceptual, 'CS-framing under-determination: extraction-grounded vs distributed-authority framings of the same regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1976, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement_basis(end__tr_t1976, observed).
narrative_ontology:measurement(end__tr_t1986, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1986, 0.28).
narrative_ontology:measurement_basis(end__tr_t1986, observed).
narrative_ontology:measurement(end__tr_t1996, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1996, 0.33).
narrative_ontology:measurement_basis(end__tr_t1996, observed).
narrative_ontology:measurement(end__tr_t2006, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2006, 0.38).
narrative_ontology:measurement_basis(end__tr_t2006, observed).
narrative_ontology:measurement(end__tr_t2016, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2016, 0.42).
narrative_ontology:measurement_basis(end__tr_t2016, observed).
narrative_ontology:measurement(end__tr_t2026, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(end__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(end__be_t1976, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1976, 0.85).
narrative_ontology:measurement_basis(end__be_t1976, observed).
narrative_ontology:measurement(end__be_t1986, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1986, 0.83).
narrative_ontology:measurement_basis(end__be_t1986, observed).
narrative_ontology:measurement(end__be_t1996, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1996, 0.79).
narrative_ontology:measurement_basis(end__be_t1996, observed).
narrative_ontology:measurement(end__be_t2006, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2006, 0.75).
narrative_ontology:measurement_basis(end__be_t2006, observed).
narrative_ontology:measurement(end__be_t2016, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2016, 0.71).
narrative_ontology:measurement_basis(end__be_t2016, observed).
narrative_ontology:measurement(end__be_t2026, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(end__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1976, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1976, 0.8).
narrative_ontology:measurement_basis(end__su_t1976, observed).
narrative_ontology:measurement(end__su_t1986, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1986, 0.78).
narrative_ontology:measurement_basis(end__su_t1986, observed).
narrative_ontology:measurement(end__su_t1996, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1996, 0.74).
narrative_ontology:measurement_basis(end__su_t1996, observed).
narrative_ontology:measurement(end__su_t2006, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement_basis(end__su_t2006, observed).
narrative_ontology:measurement(end__su_t2016, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2016, 0.66).
narrative_ontology:measurement_basis(end__su_t2016, observed).
narrative_ontology:measurement(end__su_t2026, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(end__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'end-of-life decision authority' decomposes into three structurally distinct constraints, one per reading of the kernel, per the epsilon-invariance principle. The autonomy reading (this file) authors high reading-indexed extractiveness for the shared referent — the standing medico-legal gatekeeping regime — and seats the suffering-prolonged denied access as its victim set. The sanctity sibling authors low extractiveness over the same referent and seats no denied-access victim class (prohibition is its point, not its cost). The vulnerability_protection sibling authors moderate extractiveness and seats both the denied and the coerced. The upstream sibling is sanctity: it historically supplies the doctrinal warrant the standing regime cites, and its claim is the one this reading and the vulnerability_protection reading both descend from and contest. All three files link one another via network.affects_constraints; no story in the family should be read alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
