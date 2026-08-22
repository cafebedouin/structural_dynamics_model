% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Equality as Universal Principle Requiring Iterative Expansion
 *   domain: constitutional/political_philosophy
 *
 * SUMMARY:
 *   The universalist reading of the Declaration's equality principle treats
 *   it as a universal commitment that transcends founder intent and requires
 *   iterative expansion. It reads 'all men are created equal' as an evolving
 *   principle whose scope expands as moral understanding advances and
 *   previously excluded groups mobilize to claim inclusion. This reading sits
 *   in active contestation with originalist readings (which bound equality to
 *   18th-century social taxonomy) and textualist readings (which expose the
 *   performative contradiction between universal language and restricted
 *   application). The universalist reading has dominated institutional law
 *   for the last 70 years but faces recurring originalist resistance and
 *   renewed narrowing attempts. The constraint's extractiveness is moderate
 *   (0.48) rather than high because expansion carries real coordination costs
 *   (constitutional amendment, legislative mobilization, institutional
 *   resistance) alongside benefits to the expanding group; suppression is
 *   substantial (0.62) because maintaining the universalist reading against
 *   originalist resistance requires sustained institutional and social
 *   pressure.
 *
 * KEY AGENTS:
 *   - Historically excluded groups: the beneficiaries of the universalist reading, mobilizing to claim inclusion under the principle's universal language.
 *   - Privileged status holders: those whose advantages rested on restricted equality, bearing the costs of expansion.
 *   - Constitutional originalists: the institutional opposition, defending founder-intent bounds against universalist expansion.
 *   - Universalist constitutional interpreters: the institutional advocates, reading equality as living and progressive.
 *   - Textualist paradox readers: the analytical observers, exposing the irreconcilability of universal language with restricted application.
 *   - Legislative majorities: the agents of legal settlement, enacting expansion or resistance.
 *   - Social movements: the political force driving expansion, formally excluded but institutionally consequential through pressure.
 *   - International human rights bodies: the reinforcing institutional layer, amplifying universalist framing globally.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.48).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.62).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Equality as Universal Principle Requiring Iterative Expansion").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'dc4b177d-3937-49bd-abcc-e4c47cb17b51').
narrative_ontology:cs_kernel_codification('dc4b177d-3937-49bd-abcc-e4c47cb17b51', fixed_text).
narrative_ontology:cs_authority_grounding('dc4b177d-3937-49bd-abcc-e4c47cb17b51', lineage).
narrative_ontology:cs_interpretation_layer_present('dc4b177d-3937-49bd-abcc-e4c47cb17b51').
narrative_ontology:cs_reading_relation('dc4b177d-3937-49bd-abcc-e4c47cb17b51', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc4b177d-3937-49bd-abcc-e4c47cb17b51', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('dc4b177d-3937-49bd-abcc-e4c47cb17b51', foundational, equality_principle_self_executing_over_time).
narrative_ontology:cs_axiom_status(equality_principle_self_executing_over_time, holdable).
narrative_ontology:cs_axiom_grounding('dc4b177d-3937-49bd-abcc-e4c47cb17b51', equality_principle_self_executing_over_time, deontological).
narrative_ontology:cs_axiom('dc4b177d-3937-49bd-abcc-e4c47cb17b51', foundational, moral_understanding_expands_constitutional_scope).
narrative_ontology:cs_axiom_status(moral_understanding_expands_constitutional_scope, holdable).
narrative_ontology:cs_axiom_grounding('dc4b177d-3937-49bd-abcc-e4c47cb17b51', moral_understanding_expands_constitutional_scope, empirically_contingent).
narrative_ontology:cs_axiom('dc4b177d-3937-49bd-abcc-e4c47cb17b51', secondary, founder_intent_not_binding_on_principle_scope).
narrative_ontology:cs_axiom_status(founder_intent_not_binding_on_principle_scope, holdable).
narrative_ontology:cs_axiom_grounding('dc4b177d-3937-49bd-abcc-e4c47cb17b51', founder_intent_not_binding_on_principle_scope, deontological).
narrative_ontology:cs_reference_frame('dc4b177d-3937-49bd-abcc-e4c47cb17b51', universal_moral_status_progressively_realized).
narrative_ontology:cs_drift_state('dc4b177d-3937-49bd-abcc-e4c47cb17b51', contemporary_post_civil_rights, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc4b177d-3937-49bd-abcc-e4c47cb17b51', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, rights_expansion_constituencies).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, privileged_status_holders).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, constitutional_originalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, universalist_constitutional_interpreters).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, human_equality_transcends_founder_intent).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, constitutional_purpose_evolves_with_moral_understanding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women, enslaved persons, racial minorities, LGBTQ individuals, religious minorities — all groups initially excluded from the equality principle's scope. They mobilize the universalist reading as a legitimacy claim for inclusion and remedies. The universalist frame provides moral and constitutional leverage to contest historical exclusion and demand equal status and protection. Their victory in court or legislation expands who the principle protects; their loss locks them out another generation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Those whose structural advantages (political power, property claims, social hierarchy, legal privileges) rested on the restricted scope of equality. They bear the costs of expansion: lost monopolies on citizenship, legal standing, property, voting, marriage, workplace participation, and social authority. They resist the universalist reading as a threat to stability and constitutional meaning. Their sustained resistance creates the suppression dynamic — legal battles, legislative entrenchment, state constitutional amendments designed to prevent reversal.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, privileged_status_holders, payer,
    powerful, biographical, constrained, national).

% Judges, scholars, and political actors who interpret the Constitution as fixed by founder intent and ratified meaning. They defend the originalist reading against the universalist claim, arguing that equality is bounded by 18th-century social taxonomy and that reading it as universally expansive is judicial overreach. They set the agenda in courts and constitutional scholarship by framing the contest as one of fidelity to text and history versus progressive rewriting. They can theoretically switch positions but are invested in the originalist frame.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_originalists, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, constitutional_originalists, agenda_setter).

% Judges, scholars, and political actors who read the equality principle as universal and self-executing over time — a living commitment that each generation must expand as moral understanding and social inclusion grow. They argue the principle's meaning cannot be frozen at its origin and that iterative expansion is fidelity to its fundamental aspirations. They set competing agenda in courts and academia. They hold the preponderance of institutional power in elite institutions.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, universalist_constitutional_interpreters, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, universalist_constitutional_interpreters, agenda_setter).

% A smaller set of scholars and jurists who read the universal language of the equality claim as irreconcilable with restricted application — creating a performative contradiction that destabilizes both originalist and universalist readings. They observe the contest between the other readings but do not occupy a stable seat in the political/judicial process. Their role is analytical exposure of the tension rather than advocacy for a stable resolution.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, textualist_paradox_readers, observer,
    institutional, generational, analytical, national).

% Congress and state legislatures that enact statutes implementing or resisting the equality principle — Civil Rights Acts, voting rights legislation, antidiscrimination laws — or that pass constitutional amendments enshrining expansions or attempting reversals. They are the agent through which legal/political settlement happens, though they often follow judicial determination or are forced by social pressure. Their legislative choices lock in expansions or enable rollback.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, legislative_majorities, agenda_setter,
    institutional, biographical, mobile, national).

% Protest movements, civil rights organizing, women's suffrage campaigns, LGBTQ+ activism, labor movements — mobilizing communities to pressure courts and legislatures toward the universalist reading. Formally excluded from constitutional adjudication and legislative chambers, but their mobilization creates the political conditions that make institutional actors responsive to the universalist frame. Without their voice and pressure, judicial and legislative expansion stalls or reverses.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, social_movements, excluded,
    organized, biographical, constrained, national).

% UN bodies, international courts, and transnational advocacy networks that recognize and reinforce the universalist reading of human equality. They observe the American constitutional contest from outside but also amplify the universalist frame through treaty obligations and soft-law pressure, adding legitimacy and institutional weight to claims for expansion.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, universalist_constitutional_interpreters).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared normative commitment that persons possess equal moral and legal status, and that this status must be progressively realized as moral understanding and social inclusion advance. Coordinates successive generations of legal interpretation, political mobilization, and constitutional amendment around the proposition that equality is not frozen but living.
% TRANSFER_FUNCTION: Moves legal status, political power, property rights, and social authority from those who initially monopolized them (privileged groups) toward those initially excluded. The constraint redistributes who may claim protection under law, who may vote, who may hold property, who may marry, who may work — a wholesale reallocation of legal standing that carries massive distributional consequences.
% ABSENT_VOICES: Future generations unable to articulate what equality will require; other nations' understandings of equality that might enrich the principle but are excluded by American constitutional insularity; those outside the national boundary whom equal status would compel to recognize (stateless persons, non-citizens, the globally poor) remain structurally absent from the legal conversation. Social movements are formally excluded from constitutional chambers despite being politically consequential.
% DISAPPEARANCE_RATIONALE: If the universalist reading vanished overnight and equality reverted to originalist bounds, legal status would be restructured around founder-era taxonomy — women would lose recognized political personhood, minorities would lose formal equal protection, LGBTQ individuals would lose marriage equality and workplace protection. Political power would reconcentrate among those historically privileged; property and inheritance laws would revert to gendered and racial hierarchies; suffrage would shrink to early-republic boundaries. The entire institutional and social order built atop expansions would undergo reorganization.
% FOUNDING_PROBLEM: The Declaration and Constitution contain universal language ('all men are created equal', 'equal protection of the laws') while the institutional reality enforces a restricted taxonomy. This contradiction — the gap between universal assertion and restricted application — creates a site of moral and legal contestation that each excluded group can exploit to claim inclusion as the principle's true meaning rather than its expansion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by every rights-expansion movement in American history: abolitionists, suffragists, civil rights activists, and LGBTQ+ advocates all point to the universal language as the kernel of their claim and treat the exclusions as failures to honor the principle's own logic. Historians and political theorists from outside the originalist establishment (Thomas Jefferson himself acknowledged the tension; Lincoln weaponized it to frame slavery as a violation of the Declaration's own foundation; contemporary scholars attest the contradiction persists) corroborate that the founding problem is live and unresolved.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the universalist reading's operation requires sustained coordination and institutional mobilization; it is not a simple extraction mechanism but a live contestation whose outcome depends on political power and moral persuasion. Suppression is substantial (0.62) because originalist judges, legislatures, and networks actively resist the universalist frame through doctrinal arguments, legislative entrenchment, and constitutional amendments designed to prevent reversal. Theater is moderate-high (0.41) because the universalist reading does perform real coordination work (uniting excluded groups, legitimating their claims, providing moral language for mobilization) but increasingly relies on institutional theater (rhetorical framing in judicial opinions, symbolic legislation, ceremonial recognition) to maintain resistance to originalist framings. The measurement series shows extractiveness and suppression rising early (0–15 on the interval) as the universalist reading consolidates institutional power and exclusionary originalism is pushed to the periphery, then plateauing (15–25) as the constraint reaches a new equilibrium where originalist resistance persists but is institutionally outmatched. Theater rises steadily as the constraint's operation becomes more performative — the universalist frame is rhetorically dominant but faces recurring institutional and political attacks that require constant repetition and symbolic labor to maintain.
 *
 * PERSPECTIVAL GAP:
 *   The universalist constitutional interpreters and historically excluded groups experience the constraint as a liberating expansion and moral vindication — they experience low directionality (beneficiary end) because the principle's evolution benefits them. Privileged status holders and originalists experience it as extractive redistribution and institutional overreach — they experience high directionality (target end, computed as 0.72 for institutional originalists per the override) because they bear the costs of having their monopolies on status and power opened to others. Textualist paradox readers experience the constraint as structurally self-undermining — the gap between universal language and contested application is the whole point of their reading, and they sit outside the beneficiary/victim binary as analytical observers of the performative contradiction. The engine computes these differences from the structural data (power, exit options, role) without requiring the claim to adjudicate them; the story's claim (tangled_rope) sits at the midpoint where both coordination and extraction are real, and different seats experience the constraint's type completely differently — a classic seat divergence scenario.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups have organized power and constrained exit (identity and mobilization locked into the struggle for inclusion); they are beneficiaries (role) whose directionality derives to ~0.25 (beneficiary tuning). Privileged status holders have powerful institutional position but face mobilized organized resistance; they are payers (role) whose directionality derives to ~0.80 (target tuning). Constitutional universalists as institutional beneficiaries sit near d=0.10 (beneficiary end, mobile exit, agenda-setter power); constitutional originalists are payers (role) with high directionality. The directionality_overrides entry sets d=0.72 for institutional power atoms to reflect that originalist institutional actors are not full targets (d=1.0) but are substantially constrained by the universalist reading's institutional dominance — they can theoretically switch readings or withdraw (mobile exit) but face institutional and professional consequences for doing so. This override prevents the derivation from computing them as symmetric (which would be false) and correctly places them as high-target seats defending against institutional pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling universal language with restricted application) is live but contested. The mandatrophy scenario would be: the founding problem dies (consensus emerges that equality is fully universalized and needs no further expansion) while the institutional apparatus (courts, legislatures, executive agencies) maintaining the universalist reading persists as theater. This is not the current state — the founding problem remains genuinely contested (originalists still argue for bounded equality, some status-quo-ante movements still mobilize to prevent expansion of equality principles to new groups like trans persons or undocumented immigrants), so the constraint is not yet mandatropic. However, in jurisdictions or moments where universalist expansion is institutionally consolidated and originalist resistance is pushed to the periphery, localized mandatrophy could emerge — the universalist apparatus persists (civil rights agencies, antidiscrimination law, judicial precedent) performing the expansion work, but the political fight that justified the apparatus has been won and the constraint becomes maintenance theater. The measurement series shows early-stage mandatrophy symptoms in the plateauing of extractiveness and suppression after t=15, suggesting the constraint is entering a period where expansion momentum has slowed in some domains (racial equality, gender equality) and institutional maintenance is more prominent than fresh conflicts, though the global contest is not yet settled and new frontiers for expansion (non-binary gender, disability justice, economic equality, global migration) continue to emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universalist_boundary_perpetual_expansion,
    'Does the universalist reading logically require perpetual expansion without limit, or can it reach a terminal condition where equality is fully achieved and the constraint becomes stable?',
    'Historical trajectory: examine whether new equality claims continue to emerge indefinitely or whether past expansion cohorts reach a stable post-expansion equilibrium. Test whether the principle generates its own successor claims (intersectionality, global justice, non-human rights) or whether it can be satisfied.',
    'If expansion is perpetual, the constraint carries embedded mandatrophy risk (the apparatus persists even after the founding problem is solved); if it can terminate, the constraint describes a transitional process with an achievable endpoint. This affects the classification of the reading itself — perpetual-expansion may indicate a structural contradiction between the universalist frame and terminal stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalist_boundary_perpetual_expansion, conceptual, 'Whether the universalist reading logically implies perpetual expansion or terminal equality.').

omega_variable(
    kernel_contest_irreducibility,
    'Can the three sibling readings coexist indefinitely in institutional competition, or does one reading logically foreclose the others?',
    'Examine whether originalist, textualist, and universalist readings can occupy the same legal and political space without one ruling out the others, or whether the contest will eventually settle on one canonical reading. Test whether the performances of each reading (institutional power, persuasiveness, political mobilization) can persist in equilibrium or whether one must dominate.',
    'If readings coexist indefinitely, the constraint describes a permanent contestation state and the three readings are genuinely coexisting siblings. If one reading eventually forecloses the others, the constraint describes a transitional moment in a larger historical arc where the universal reading eventually wins and originalism becomes a historical artifact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_irreducibility, conceptual, 'Whether the kernel contest is resolvable or perpetually open.').

omega_variable(
    expansion_cost_extraction_boundary,
    'Is the measured extractiveness (0.48) tracking genuine coordination costs of expansion (legitimate transaction costs), or is it tracking the redistributive violence of taking status and power from privileged groups?',
    'Separate the constraint''s operation into coordination component (the institutional work of interpreting and implementing expansion) and redistribution component (the taking of power/status from resisters). Test whether coordination costs are symmetrical (all groups pay equally) or asymmetrical (expansion is costless to beneficiaries, expensive to losers).',
    'If the extraction is legitimate coordination cost, the tangled_rope classification (both coordination and extraction) is accurate. If the extraction is primarily redistributive violence imposed on losers, the classification might shift toward snare-like (pure extraction wearing coordination clothing). This affects how the constraint is evaluated — as fair burden-sharing or as organized taking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_cost_extraction_boundary, empirical, 'Whether measured extractiveness represents coordination cost or redistributive violence.').

omega_variable(
    originalist_reading_structural_alternative,
    'Is the originalist reading genuinely a coherent alternative constitutional frame, or is it a defensive posture adopted by those whose interests are threatened by expansion?',
    'Examine originalist scholarship and jurisprudence on their own terms: can originalism coherently interpret equality while maintaining founder-era bounds, or does the principle''s own language logically require the universalist reading? Test whether originalists apply originalism consistently to other constitutional principles or whether they deploy it selectively when expansion threatens their interests.',
    'If originalism is a coherent alternative frame, the three readings are genuinely competing constitutional interpretations and the kernel contest is real. If originalism is post-hoc rationalization for maintaining the status quo, it functions more as a payer seat defending its position than as a genuine alternative reading, and the contest is primarily about power rather than interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_reading_structural_alternative, conceptual, 'Whether originalism is a coherent reading or a defensive posture.').

omega_variable(
    suppression_internalization_structural_internalized,
    'Is the measured suppression (0.62) representing active institutional resistance (originalist judges, legislative entrenchment, executive non-enforcement), or is it partly internalized — excluded groups internalizing limiting beliefs about their own eligibility for equal status?',
    'Post-expansion trajectory: if suppression collapses after a group''s legal status is expanded and they experience non-discrimination in practice, the suppression was structural. If suppression persists among group members even after legal barriers are removed, some component is internalized. Test whether newly protected groups require generations to fully claim their expanded status or whether the transition is rapid.',
    'If suppression is primarily structural, the measured figure (0.62) represents active institutional force. If suppression is partly internalized, the effective suppression is higher than the institutional measure suggests — the constraint carries internalized limiting beliefs that persist even after formal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_structural_internalized, empirical, 'Whether suppression is structural, internalized, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__universalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(all__tr_t0, observed).
narrative_ontology:measurement(all__tr_t5, all_men_created_equal__universalist_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(all__tr_t5, observed).
narrative_ontology:measurement(all__tr_t10, all_men_created_equal__universalist_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(all__tr_t10, observed).
narrative_ontology:measurement(all__tr_t15, all_men_created_equal__universalist_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(all__tr_t15, observed).
narrative_ontology:measurement(all__tr_t20, all_men_created_equal__universalist_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(all__tr_t20, observed).
narrative_ontology:measurement(all__tr_t25, all_men_created_equal__universalist_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(all__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__universalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(all__be_t0, observed).
narrative_ontology:measurement(all__be_t5, all_men_created_equal__universalist_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(all__be_t5, observed).
narrative_ontology:measurement(all__be_t10, all_men_created_equal__universalist_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(all__be_t10, observed).
narrative_ontology:measurement(all__be_t15, all_men_created_equal__universalist_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(all__be_t15, observed).
narrative_ontology:measurement(all__be_t20, all_men_created_equal__universalist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(all__be_t20, observed).
narrative_ontology:measurement(all__be_t25, all_men_created_equal__universalist_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(all__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__universalist_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(all__su_t0, observed).
narrative_ontology:measurement(all__su_t5, all_men_created_equal__universalist_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(all__su_t5, observed).
narrative_ontology:measurement(all__su_t10, all_men_created_equal__universalist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(all__su_t10, observed).
narrative_ontology:measurement(all__su_t15, all_men_created_equal__universalist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(all__su_t15, observed).
narrative_ontology:measurement(all__su_t20, all_men_created_equal__universalist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(all__su_t20, observed).
narrative_ontology:measurement(all__su_t25, all_men_created_equal__universalist_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(all__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, voting_rights_enforcement).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, gender_equality_doctrine).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, racial_equal_protection).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, lgbtq_marriage_equality).

% DUAL FORMULATION NOTE:
% This constraint is the universalist reading of the kernel 'all_men_created_equal'. It affects and is affected by the originalist_reading and textualist_paradox_reading constraints via the shared kernel contest. It also influences downstream constraints in specific domains (voting rights, gender equality, racial equal protection, marriage equality) where the universalist reading's expansion logic plays out in particular institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
