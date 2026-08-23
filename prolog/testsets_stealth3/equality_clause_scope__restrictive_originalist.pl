% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Scope of Constitutional Equality (Propertied White Male Contracting Class)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This story instantiates one reading — restrictive_originalist — of the
 *   contested kernel equality_clause_scope: the question of whom a
 *   constitution's equality guarantee addresses. Under this reading the
 *   guarantee's scope was fixed at ratification: full political standing
 *   (franchise, officeholding, contractual personality, rights priority)
 *   attaches to propertied white males as the contracting class of the
 *   eighteenth-century social compact, and every rights claim outside that
 *   class requires its own constitutional basis won through Article V
 *   amendment. Across the 240-year interval the arrangement's original
 *   content was mostly dismantled by exactly the channel the reading honors —
 *   the Thirteenth through Nineteenth, Twenty-Fourth and Twenty-Sixth
 *   Amendments — while the reading persisted as an interpretive method
 *   setting a high legitimacy threshold before any further expansion is
 *   recognized. The ε referent is the standing restrictive arrangement as
 *   this reading itself conceives it: the reading concedes the frame
 *   concentrates political standing in one class and prices every expansion
 *   claim onto the amendment path; what it disputes is the justice of calling
 *   that arrangement illegitimate. Sibling readings (expansive_universalist,
 *   progressive_textualist) are separate constraint stories assessing the
 *   same referent with their own reading-indexed ε values. KEY AGENTS (by
 *   structural relationship): - propertied_white_male_citizens: Primary
 *   beneficiary (institutional/arbitrage) — the contracting class holding
 *   exclusive political standing - enslaved_black_people: Primary target
 *   (powerless/trapped) — held as property inside the settlement that
 *   pronounced men equal - free_black_people: Target (powerless/trapped) —
 *   resident without citizenship or franchise - women: Target
 *   (powerless/identity_locked) — legal personhood folded into husbands under
 *   coverture - nonpropertied_men: Target (moderate/constrained) — taxed and
 *   counted but vote-barred; uniquely able to win inclusion through ordinary
 *   state politics - indigenous_peoples: Target (powerless/trapped) —
 *   polities treated as foreign bodies inside allocated space -
 *   federal_judiciary: Agenda-setter (institutional/constrained) —
 *   administers the scope case by case - originalist_legal_movement:
 *   Agenda-setter and beneficiary (institutional/identity_locked) — supplies
 *   and staffs the doctrine - expansion_advocates: Organized opposition
 *   bearing the threshold's costs (organized/mobile) -
 *   constitutional_historians: Analytical observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - - propertied_white_male_citizens: Primary beneficiary (institutional/arbitrage) — holds the exclusive political standing the frame confers and controls the amendment channel
 *   - - enslaved_black_people: Primary target (powerless/trapped) — labor and persons priced into the settlement; exit required armed force
 *   - - free_black_people: Target (powerless/trapped) — resident in the republic without its guarantees; emigration the only nominal exit
 *   - - women: Target (powerless/identity_locked) — coverture plus learned separate-spheres ideology; the deepest lock partly internalized
 *   - - nonpropertied_men: Target (moderate/constrained) — barrier sat in state statute, removable by ordinary politics, unlike the others
 *   - - indigenous_peoples: Target (powerless/trapped) — sovereign polities admitted only as objects of federal power, never as members
 *   - - federal_judiciary: Agenda-setter (institutional/constrained) — administers the scope; self-bound by committed methodology, escape only through slow appointment turnover
 *   - - originalist_legal_movement: Agenda-setter and beneficiary (institutional/identity_locked) — careers and institutions invested in the method's authority
 *   - - expansion_advocates: Payer (organized/mobile) — absorb the threshold's costs in lost motions and failed amendments
 *   - - constitutional_historians: Analytical observer (analytical/analytical) — produce the evidentiary record both sides must cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.41).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.38).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.41).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Scope of Constitutional Equality (Propertied White Male Contracting Class)").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, 'b04ad643-b390-4519-8a6a-b40740e066c7').
narrative_ontology:cs_kernel_codification('b04ad643-b390-4519-8a6a-b40740e066c7', fixed_text).
narrative_ontology:cs_authority_grounding('b04ad643-b390-4519-8a6a-b40740e066c7', lineage).
narrative_ontology:cs_interpretation_layer_present('b04ad643-b390-4519-8a6a-b40740e066c7').
narrative_ontology:cs_reading_relation('b04ad643-b390-4519-8a6a-b40740e066c7', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('b04ad643-b390-4519-8a6a-b40740e066c7', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('b04ad643-b390-4519-8a6a-b40740e066c7', foundational, original_scope_fixity).
narrative_ontology:cs_axiom_status(original_scope_fixity, holdable).
narrative_ontology:cs_axiom_grounding('b04ad643-b390-4519-8a6a-b40740e066c7', original_scope_fixity, conventional).
narrative_ontology:cs_axiom('b04ad643-b390-4519-8a6a-b40740e066c7', foundational, amendment_exclusive_expansion_channel).
narrative_ontology:cs_axiom_status(amendment_exclusive_expansion_channel, holdable).
narrative_ontology:cs_axiom_grounding('b04ad643-b390-4519-8a6a-b40740e066c7', amendment_exclusive_expansion_channel, instrumental).
narrative_ontology:cs_reference_frame('b04ad643-b390-4519-8a6a-b40740e066c7', ratification_era_contractual_scope).
narrative_ontology:cs_drift_state('b04ad643-b390-4519-8a6a-b40740e066c7', contemporary_post_amendment_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b04ad643-b390-4519-8a6a-b40740e066c7', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_black_people).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_black_people).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, nonpropertied_men).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_legal_movement).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, expansion_advocates).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_jurisprudence).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, article_v_amendment_exclusivity).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, founders_constitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constituted the ratifying and voting class the founding settlement addressed: they alone held franchise, officeholding eligibility, and full contractual personality. Every benefit of the equality guarantee flows to them first, and they control the amendment process, so rewriting the rules is always available from where they stand — leaving the arrangement never arises as a question.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, beneficiary,
    institutional, generational, arbitrage, national).

% Held as property under the same constitutional order that pronounced men equal. The apportionment clause, fugitive slave provisions, and state police powers priced their labor and persons into the settlement. Flight, rebellion, and petition were answered with patrols, bounties, and legal reprisal; no procedural route out existed until armed force broke the arrangement.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_black_people, payer,
    powerless, biographical, trapped, national).

% Lived inside the republic without its guarantees: barred from voting in most states, from testifying against whites, and — by judicial ruling at mid-century — from citizenship itself. Free states competed in excluding them; colonization schemes were the only nominal exits and most could not take them.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_black_people, payer,
    powerless, biographical, trapped, national).

% Married women stood in coverture: legal personhood folded into a husband's, wages his, domicile his. Single women could hold property in some states but could not vote or hold office. Generations were taught that political claims were unfeminine, so demanding standing felt like a breach of who they were told to be — the bars were written into statute, and part of the lock was installed as self-understanding.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, biographical, identity_locked, national).

% White men without land paid taxes, served in militias, and were counted for representation while most state constitutions reserved the vote to taxpayers or freeholders. Unlike the other excluded groups they could win inclusion through ordinary state politics — and did, state by state, in the Jacksonian years — because the barrier they faced sat in statute rather than in the founding text's own definition of the contracting class.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, nonpropertied_men, payer,
    moderate, biographical, constrained, national).

% Nations with their own governments, treated as foreign bodies inside the continent the settlement allocated. Plenary federal power and serial treaty-breaking removed them from lands their polities held; removal and allotment were the recurring exits offered. Their governments persist, but the constitutional frame never admitted them as equal members — only as objects of federal authority.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, payer,
    powerless, generational, trapped, continental).

% Decides case by case whether an equality or rights claim reaches constitutional force or is turned away for lacking a textual or ratification-era basis. A bench committed to the historical-scope method cannot casually switch methods without conceding its own past errors, so each justice inherits predecessors' commitments; the escape valve is slow turnover in appointments.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Supplies the doctrine: scholars, litigators, and judges who build the historical case for the founding-era scope and staff the pipeline placing its practitioners on benches. Careers, networks, and endowed institutions are invested in the method's authority; abandoning it would strand lifetimes of work, so adherence is renewed ritually in journals, at conferences, and in nomination hearings.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_legal_movement, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, originalist_legal_movement, beneficiary).

% Movements and public-interest litigators pressing claims the frame turns away. They absorb decades of lost motions, failed amendment campaigns, and state-by-state fallbacks. Their mobility is real but costly: every closed federal door reroutes them to fifty statehouses or to cultural persuasion, multiplying the price of each advance.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansion_advocates, payer,
    organized, generational, mobile, national).

% Read the ratification record, correspondence, and case law from outside the dispute, reconstructing what the founding generation understood and how the scope has been argued over since. They command no enforcement power; their product is the evidentiary record both sides of the scope dispute must cite.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single interpretive baseline for who counts as politically equal, making judicial review predictable and routing all scope changes through one supermajoritarian channel (Article V) instead of case-by-case judicial discretion — solving the problem of unstable, personality-driven constitutional meaning.
% TRANSFER_FUNCTION: Moves political standing and rights-enforcement priority from women, enslaved and free Black people, non-propertied men, and indigenous peoples to the propertied white male class; moves the cost of every proposed expansion onto claimants, who must fund amendment campaigns or absorb judicial dismissal.
% ABSENT_VOICES: At the founding the excluded had no seats: no woman, no enslaved person, no propertyless man sat in a ratifying convention, and indigenous nations bargained from outside as foreign polities. Their objections entered the record only secondhand, through abolitionist and suffrage presses — never through the consent procedures the frame invokes. Today their successors appear as litigants and movements (expansion_advocates), still without any seat that can veto the threshold itself.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, doctrines keyed to founding-era scope — denials of unenumerated rights, restrictive protected-class thresholds, original-meaning tests — would lose their warrant; expansion claims currently routed to Article V would return to the courts immediately; and the interpretive coalitions organized around the method would dissolve or re-form around a sibling reading. Institutions are arranged around it, so the world rearranges.
% FOUNDING_PROBLEM: Legitimating and stabilizing a continental republic whose compact was drafted and ratified exclusively by propertied white men: deciding, implicitly, who the parties to the social contract were, and holding that decision fixed so ordinary politics could operate inside it.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: ratification-era convention records and Federalist-era correspondence attest the scope as its framers understood it; Frederick Douglass's 1852 Fourth of July address attests the narrowness as lived; the Seneca Falls Declaration attests women's exclusion; the Dred Scott ruling attests it as enforced. On status, the parties genuinely dispute it: originalist scholars attest that the interpretive-stability problem the arrangement solved remains live, while social and legal historians attest that the party-composition problem died with the founding generation — that dispute is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.41, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).
:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts: I claim tangled_rope because the arrangement possesses a genuine coordination function (a fixed interpretive baseline plus a single supermajoritarian expansion channel, solving the problem of personality-driven constitutional meaning) AND asymmetric extraction through the same structure (one class holds standing; excluded groups pay), actively enforced. The metrics describe operation as the record shows it. Extraction starts high (0.74) at ratification, dips at t80 (circa 1867, when the Reconstruction Amendments stripped the frame's core exclusions), rebounds at t120 (circa 1907, when Jim Crow restored the old scope de facto), and decays to 0.41 as formal exclusions were amended away and the residual cost narrowed to blocked expansion claims. That trough-and-rebound is a genuine cycle driven by constitutional-politics regimes (founding settlement -> Civil War rupture -> Redemption counterrevolution -> civil-rights dismantling), not intermittent reinforcement; it is documented here because it dates the type-relevant transitions. Suppression_requirement is tracked because enforcement capacity is the dynamic: a purpose-built apparatus (patrols, fugitive clauses, coverture law) peaked around t40, was war-destroyed at t80, rebuilt through terror and poll taxes at t120, then progressively replaced by purely doctrinal enforcement. Theater rises from 0.22 (the founding's professed-universal-versus-practiced-scoped gap Douglass named) through the Jim Crow peak (0.55: professed post-Fourteenth-Amendment equality over practiced apartheid) to a steady ~0.48 today — fidelity rituals performed around a frame whose operative content has been mostly amended away. Suppression is authored as a raw structural property; only extractiveness gets scaled by directionality and scope downstream. All three series share one grid (t = 0, 40, 80, 120, 160, 200, 240, years since 1787 ratification; t240 circa 2027, marked projected) so no end-state value leaks backward into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. The propertied-white-male seat experiences the arrangement as inherited common sense — a coordination it did not choose but inhabits, with rewrite-always-available arbitrage. The trapped victim seats (enslaved and free Black people, indigenous peoples) experience the same structure as enforced exclusion with no procedural exit; the identity_locked women's seat experiences a bar that was partly learned as self-definition. The judiciary seat experiences methodological discipline — each justice inheriting predecessors' commitments — while the movement seat experiences professional identity investment that makes revision personally costly. Expansion advocates experience a toll booth: every advance purchasable only at amendment-scale prices. None of these perceptions is authored as classification; the engine derives per-seat classifications from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration places propertied_white_male_citizens near the beneficiary pole (d approaching 0): the frame subsidizes them with standing and they hold arbitrage-grade control of the amendment channel. Victim declarations place the excluded groups near the target pole, modulated by exit: enslaved_black_people and free_black_people (trapped) sit nearest full-target; women (identity_locked) close behind, the lock fusing legal subsumption with internalized ideology; indigenous_peoples (trapped, continental scope) similarly deep. Nonpropertied_men derive a somewhat lower d: their constrained exit proved negotiable through ordinary state politics, which the derivation reads as partial mobility. The agenda-setter seats split: the judiciary administers without collecting much directly (mixed, mid-range), while the originalist_legal_movement — agenda-setter with a secondary beneficiary role and identity_locked exit — derives a lower d reflecting its collected professional gains. Expansion_advocates bear the threshold's costs (payer) but their mobility dampens effective targeting below the trapped seats. Scope runs national (continental for indigenous peoples), which amplifies effective extraction for targets modestly since verification of equal treatment across that span is hard. Suppression enters unscaled, per its status as a raw structural property.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating and stabilizing a compact whose parties were decided once, at ratification — has largely outlived its function: no living governance task requires re-deciding who the eighteenth-century contracting class was. What persists is (a) a genuine residual function, stabilizing interpretation and gating expansion behind supermajority consent, and (b) a thick layer of performative maintenance — conferences, journals, confirmation hearings renewing fidelity to a frame whose original content is mostly repealed. The theater_ratio trajectory (rising from 0.22 to a sustained ~0.48) is the measurable symptom. The R5 interview records the founding problem's status as contested (originalists attest the stabilization problem is live; social historians attest the party-composition problem is dead), which pairs with a world_rearranges disappearance verdict — arrangements demonstrably depend on the reading, so no automatic zombie flag fires, but the theater path and the rising structural-level threshold in the coercion grid are where decay would surface first. The classification prevents two symmetrical mislabels: calling the arrangement pure coordination ignores the named victims and the enforcement history; calling it pure extraction ignores the functioning amendment channel the reading itself honors — the very channel that dismantled most of its original content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (restrictive_originalist) of the kernel equality_clause_scope; would instantiating expansive_universalist or progressive_textualist instead dissolve this story''s beneficiary/victim structure?',
    'Compare the sibling stories'' computed structures. The disagreement is located in one element: whether the clause''s scope is historically fixed (this reading), self-universalizing (expansive_universalist), or amendment-grown through an embedded principle (progressive_textualist).',
    'Under the universalist reading the excluded become covered and this story''s victim set empties into the sibling''s; under the textualist reading the expansion threshold softens into democratic-process legitimacy. This story''s epsilon, victims, and threshold are valid only within the restrictive reading — they are not claims about the other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one of three readings of a contested constitutional kernel.').

omega_variable(
    constructed_vs_inherited_exclusion,
    'Was the restriction of equality to propertied white males a deliberate design choice serving that class, or an unreflective background category of the era adopted without exclusionary intent?',
    'Archival work on the drafting and ratification of the franchise and apportionment provisions: explicit deliberation over who counts would indicate design; silence would indicate inherited assumption.',
    'Deliberate design pushes the arrangement toward pure extraction with the coordination story as cover; unreflective inheritance makes it inertial — a frame maintained by habit and coalition investment rather than by anyone''s ongoing design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_inherited_exclusion, empirical, 'Whether the founding scope was engineered exclusion or era-default.').

omega_variable(
    amendment_threshold_function,
    'Is the supermajoritarian amendment requirement a genuine coordination safeguard against judicial caprice, or a suppression mechanism that entrenches whichever coalition holds the status quo?',
    'Comparative analysis of amendment success rates for expansion claims versus entrenchment claims across the interval, plus counterfactual comparison with jurisdictions having easier amendment paths.',
    'A genuine safeguard sustains the coordination half of the hybrid classification; an entrenchment device strips the coordination function and leaves enforcement-backed exclusion as the whole of what persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_threshold_function, conceptual, 'Coordination-safeguard versus entrenchment reading of the Article V gate.').

omega_variable(
    movement_identity_fusion,
    'Is the originalist legal movement''s adherence evidence-responsive (systematic contrary evidence would move it) or identity-fused (career, network, and institutional investment make exit unthinkable)?',
    'Track the movement''s doctrinal response to disconfirming historical scholarship and to adverse appointment politics; examine career-incentive structures in the pipeline institutions.',
    'Evidence-responsive adherence keeps the agenda-setter seat revisable and the arrangement corrigible; identity fusion hardens it, raising the persistence of the threshold independently of its merits and pushing the arrangement toward degraded persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_identity_fusion, empirical, 'Professional identity fusion in the interpretive coalition that administers the reading.').

omega_variable(
    residual_extraction_boundary,
    'Do present-day denials of unenumerated-rights and newly-asserted-class claims (which the end-state epsilon counts) constitute extraction by this reading, or mere non-application of a text that never promised those rights?',
    'Doctrinal analysis distinguishing refusal-to-extend from affirmative withdrawal, and comparison with jurisdictions that recognize the same claims through other instruments.',
    'As extraction, the end-state epsilon is understated and the arrangement remains materially costly to identifiable groups; as non-application, the residual cost is opportunity forgone rather than value transferred, and the reading''s present footprint is mostly threshold friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_extraction_boundary, conceptual, 'Boundary of what counts as the reading''s present-day taking versus its simple non-reach.').

omega_variable(
    coercion_grid_level_uncertainty,
    'The coercion_grid''s level-resolved values are conservative reconstructions from qualitative histories; how far do they misstate per-level intensities, particularly class-level resistance at t0 and organizational-level suppression at tn?',
    'Quantitative archival work: prosecution and vigilante-violence rates (individual level), enfranchisement-campaign records (class level), court and party behavior (organizational level).',
    'Refinement could sharpen or flatten the encoded level gradient; the robust finding is the migration pattern — individual-level suppression collapsing across the interval while the structural-level amendment threshold hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_grid_level_uncertainty, empirical, 'Uncertainty attached to level-resolved coercion judgments in the authored grid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eq_scope_restr_tr_t0, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(eq_scope_restr_tr_t0, observed).
narrative_ontology:measurement(eq_scope_restr_tr_t40, equality_clause_scope__restrictive_originalist, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(eq_scope_restr_tr_t40, observed).
narrative_ontology:measurement(eq_scope_restr_tr_t80, equality_clause_scope__restrictive_originalist, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(eq_scope_restr_tr_t80, observed).
narrative_ontology:measurement(eq_scope_restr_tr_t120, equality_clause_scope__restrictive_originalist, theater_ratio, 120, 0.55).
narrative_ontology:measurement_basis(eq_scope_restr_tr_t120, observed).
narrative_ontology:measurement(eq_scope_restr_tr_t160, equality_clause_scope__restrictive_originalist, theater_ratio, 160, 0.5).
narrative_ontology:measurement_basis(eq_scope_restr_tr_t160, observed).
narrative_ontology:measurement(eq_scope_restr_tr_t200, equality_clause_scope__restrictive_originalist, theater_ratio, 200, 0.46).
narrative_ontology:measurement_basis(eq_scope_restr_tr_t200, observed).
narrative_ontology:measurement(eq_scope_restr_tr_t240, equality_clause_scope__restrictive_originalist, theater_ratio, 240, 0.48).
narrative_ontology:measurement_basis(eq_scope_restr_tr_t240, projected).

% Extraction over time
narrative_ontology:measurement(eq_scope_restr_be_t0, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.74).
narrative_ontology:measurement_basis(eq_scope_restr_be_t0, observed).
narrative_ontology:measurement(eq_scope_restr_be_t40, equality_clause_scope__restrictive_originalist, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(eq_scope_restr_be_t40, observed).
narrative_ontology:measurement(eq_scope_restr_be_t80, equality_clause_scope__restrictive_originalist, base_extractiveness, 80, 0.52).
narrative_ontology:measurement_basis(eq_scope_restr_be_t80, observed).
narrative_ontology:measurement(eq_scope_restr_be_t120, equality_clause_scope__restrictive_originalist, base_extractiveness, 120, 0.67).
narrative_ontology:measurement_basis(eq_scope_restr_be_t120, observed).
narrative_ontology:measurement(eq_scope_restr_be_t160, equality_clause_scope__restrictive_originalist, base_extractiveness, 160, 0.58).
narrative_ontology:measurement_basis(eq_scope_restr_be_t160, observed).
narrative_ontology:measurement(eq_scope_restr_be_t200, equality_clause_scope__restrictive_originalist, base_extractiveness, 200, 0.44).
narrative_ontology:measurement_basis(eq_scope_restr_be_t200, observed).
narrative_ontology:measurement(eq_scope_restr_be_t240, equality_clause_scope__restrictive_originalist, base_extractiveness, 240, 0.41).
narrative_ontology:measurement_basis(eq_scope_restr_be_t240, projected).

% Suppression requirement over time
narrative_ontology:measurement(eq_scope_restr_su_t0, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.82).
narrative_ontology:measurement_basis(eq_scope_restr_su_t0, observed).
narrative_ontology:measurement(eq_scope_restr_su_t40, equality_clause_scope__restrictive_originalist, suppression_requirement, 40, 0.85).
narrative_ontology:measurement_basis(eq_scope_restr_su_t40, observed).
narrative_ontology:measurement(eq_scope_restr_su_t80, equality_clause_scope__restrictive_originalist, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(eq_scope_restr_su_t80, observed).
narrative_ontology:measurement(eq_scope_restr_su_t120, equality_clause_scope__restrictive_originalist, suppression_requirement, 120, 0.78).
narrative_ontology:measurement_basis(eq_scope_restr_su_t120, observed).
narrative_ontology:measurement(eq_scope_restr_su_t160, equality_clause_scope__restrictive_originalist, suppression_requirement, 160, 0.66).
narrative_ontology:measurement_basis(eq_scope_restr_su_t160, observed).
narrative_ontology:measurement(eq_scope_restr_su_t200, equality_clause_scope__restrictive_originalist, suppression_requirement, 200, 0.45).
narrative_ontology:measurement_basis(eq_scope_restr_su_t200, observed).
narrative_ontology:measurement(eq_scope_restr_su_t240, equality_clause_scope__restrictive_originalist, suppression_requirement, 240, 0.38).
narrative_ontology:measurement_basis(eq_scope_restr_su_t240, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=240
narrative_ontology:measurement(eq_scope_restr_grid_01, equality_clause_scope__restrictive_originalist, accessibility_collapse(class), 0, 0.75).
narrative_ontology:measurement(eq_scope_restr_grid_02, equality_clause_scope__restrictive_originalist, accessibility_collapse(class), 240, 0.52).
narrative_ontology:measurement(eq_scope_restr_grid_03, equality_clause_scope__restrictive_originalist, accessibility_collapse(individual), 0, 0.7).
narrative_ontology:measurement(eq_scope_restr_grid_04, equality_clause_scope__restrictive_originalist, accessibility_collapse(individual), 240, 0.5).
narrative_ontology:measurement(eq_scope_restr_grid_05, equality_clause_scope__restrictive_originalist, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(eq_scope_restr_grid_06, equality_clause_scope__restrictive_originalist, accessibility_collapse(organizational), 240, 0.45).
narrative_ontology:measurement(eq_scope_restr_grid_07, equality_clause_scope__restrictive_originalist, accessibility_collapse(structural), 0, 0.6).
narrative_ontology:measurement(eq_scope_restr_grid_08, equality_clause_scope__restrictive_originalist, accessibility_collapse(structural), 240, 0.78).
narrative_ontology:measurement(eq_scope_restr_grid_09, equality_clause_scope__restrictive_originalist, resistance(class), 0, 0.3).
narrative_ontology:measurement(eq_scope_restr_grid_10, equality_clause_scope__restrictive_originalist, resistance(class), 240, 0.6).
narrative_ontology:measurement(eq_scope_restr_grid_11, equality_clause_scope__restrictive_originalist, resistance(individual), 0, 0.2).
narrative_ontology:measurement(eq_scope_restr_grid_12, equality_clause_scope__restrictive_originalist, resistance(individual), 240, 0.45).
narrative_ontology:measurement(eq_scope_restr_grid_13, equality_clause_scope__restrictive_originalist, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(eq_scope_restr_grid_14, equality_clause_scope__restrictive_originalist, resistance(organizational), 240, 0.72).
narrative_ontology:measurement(eq_scope_restr_grid_15, equality_clause_scope__restrictive_originalist, resistance(structural), 0, 0.05).
narrative_ontology:measurement(eq_scope_restr_grid_16, equality_clause_scope__restrictive_originalist, resistance(structural), 240, 0.8).
narrative_ontology:measurement(eq_scope_restr_grid_17, equality_clause_scope__restrictive_originalist, stakes_inflation(class), 0, 0.86).
narrative_ontology:measurement(eq_scope_restr_grid_18, equality_clause_scope__restrictive_originalist, stakes_inflation(class), 240, 0.3).
narrative_ontology:measurement(eq_scope_restr_grid_19, equality_clause_scope__restrictive_originalist, stakes_inflation(individual), 0, 0.88).
narrative_ontology:measurement(eq_scope_restr_grid_20, equality_clause_scope__restrictive_originalist, stakes_inflation(individual), 240, 0.28).
narrative_ontology:measurement(eq_scope_restr_grid_21, equality_clause_scope__restrictive_originalist, stakes_inflation(organizational), 0, 0.7).
narrative_ontology:measurement(eq_scope_restr_grid_22, equality_clause_scope__restrictive_originalist, stakes_inflation(organizational), 240, 0.32).
narrative_ontology:measurement(eq_scope_restr_grid_23, equality_clause_scope__restrictive_originalist, stakes_inflation(structural), 0, 0.65).
narrative_ontology:measurement(eq_scope_restr_grid_24, equality_clause_scope__restrictive_originalist, stakes_inflation(structural), 240, 0.7).
narrative_ontology:measurement(eq_scope_restr_grid_25, equality_clause_scope__restrictive_originalist, suppression(class), 0, 0.88).
narrative_ontology:measurement(eq_scope_restr_grid_26, equality_clause_scope__restrictive_originalist, suppression(class), 240, 0.22).
narrative_ontology:measurement(eq_scope_restr_grid_27, equality_clause_scope__restrictive_originalist, suppression(individual), 0, 0.85).
narrative_ontology:measurement(eq_scope_restr_grid_28, equality_clause_scope__restrictive_originalist, suppression(individual), 240, 0.18).
narrative_ontology:measurement(eq_scope_restr_grid_29, equality_clause_scope__restrictive_originalist, suppression(organizational), 0, 0.75).
narrative_ontology:measurement(eq_scope_restr_grid_30, equality_clause_scope__restrictive_originalist, suppression(organizational), 240, 0.3).
narrative_ontology:measurement(eq_scope_restr_grid_31, equality_clause_scope__restrictive_originalist, suppression(structural), 0, 0.82).
narrative_ontology:measurement(eq_scope_restr_grid_32, equality_clause_scope__restrictive_originalist, suppression(structural), 240, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, progressive_textualist).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional equality' decomposes into a three-story constraint family over the kernel equality_clause_scope: this restrictive_originalist story (narrow fixed scope, high expansion threshold), expansive_universalist (self-applying universal scope), and progressive_textualist (embedded principle grown through amendment). Per the epsilon-invariance principle these are distinct constraints, not one constraint viewed from angles: each story carries its own epsilon, beneficiary/victim structure, and classification. All three assess the same standing arrangement — the restrictive regime — with reading-indexed epsilons; the sibling files author theirs. Family linkage runs through affects_constraints in all three files; the epistemic gradient runs from this reading's dense ratification-record anchoring toward the siblings' more contested scope claims, which are frequently argued BY citation to (or against) this reading's historical materials.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
