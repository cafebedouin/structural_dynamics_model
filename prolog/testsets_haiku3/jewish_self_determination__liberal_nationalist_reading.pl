% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish National Self-Determination (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the LIBERAL NATIONALIST READING of the
 *   contested kernel 'jewish_self_determination.' Under this reading, Jewish
 *   people constitute a people (a collective with shared identity, history,
 *   and cultural reference) with the same claim to national
 *   self-determination as other peoples — a position grounded in 19th and
 *   20th century liberal internationalism and the principle that peoples have
 *   the right to self-govern. The reading brackets out competing claims: it
 *   does not rest on the indigenous-return claim (unbroken historical
 *   connection), the religious-covenant claim (divine right), or the
 *   diasporist counter-claim (that statehood is unnecessary and dangerous).
 *   It rests on a universal principle: peoples have equal rights. The
 *   constraint's structure is ROPE (genuine coordination solution to diaspora
 *   insecurity) with modest extractiveness (the coordination is assumed
 *   mutual — partition into two states) and moderate resistance (from those
 *   who reject the premise or the implementation). The measurement series
 *   track how the constraint's extractiveness and suppression requirement
 *   have drifted as implementation has diverged from theory: early
 *   ideological clarity (low theater) gives way to accumulated security
 *   operations, settlement, and occupation that the reading itself does not
 *   endorse, raising theater and suppression costs to maintain the claim.
 *
 * KEY AGENTS:
 *   - jewish_diaspora: globally dispersed communities seeking refuge and equal standing
 *   - jewish_sovereignty_seekers: institutional actors (Zionist organizations, Israeli state) claiming and executing the territorial claim
 *   - palestinians_under_partition_theory: treated by this reading as co-beneficiary of partition (equal claim to self-determination), though the reading does not engage whether partition actually secures mutual coordination or produces asymmetric outcomes
 *   - diasporist_jewish_intellectuals: excluded from the frame; they argue diaspora insecurity is not structural and statehood is a misdirected response
 *   - indigenous_return_theorists: excluded; they ground the claim in historical primacy, not universal liberal principle
 *   - international_observers: assess whether the reading's claim to universal principle accords with actual implementation and international law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.45).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.38).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish National Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'a712dd0c-9a51-43f8-ac8f-8098928f108b').
narrative_ontology:cs_kernel_codification('a712dd0c-9a51-43f8-ac8f-8098928f108b', distributed).
narrative_ontology:cs_authority_grounding('a712dd0c-9a51-43f8-ac8f-8098928f108b', distributed).
narrative_ontology:cs_reading_relation('a712dd0c-9a51-43f8-ac8f-8098928f108b', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a712dd0c-9a51-43f8-ac8f-8098928f108b', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('a712dd0c-9a51-43f8-ac8f-8098928f108b', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('a712dd0c-9a51-43f8-ac8f-8098928f108b', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('a712dd0c-9a51-43f8-ac8f-8098928f108b', foundational, peoples_have_equal_self_determination_rights).
narrative_ontology:cs_axiom_status(peoples_have_equal_self_determination_rights, holdable).
narrative_ontology:cs_axiom_grounding('a712dd0c-9a51-43f8-ac8f-8098928f108b', peoples_have_equal_self_determination_rights, deontological).
narrative_ontology:cs_axiom('a712dd0c-9a51-43f8-ac8f-8098928f108b', foundational, partition_solution_ensures_mutual_coordination).
narrative_ontology:cs_axiom_status(partition_solution_ensures_mutual_coordination, holdable).
narrative_ontology:cs_axiom_grounding('a712dd0c-9a51-43f8-ac8f-8098928f108b', partition_solution_ensures_mutual_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('a712dd0c-9a51-43f8-ac8f-8098928f108b', universal_liberal_nationalist_principle).
narrative_ontology:cs_drift_state('a712dd0c-9a51-43f8-ac8f-8098928f108b', contemporary_implementation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a712dd0c-9a51-43f8-ac8f-8098928f108b', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_sovereignty_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, palestinians_under_partition_theory).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, international_liberal_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed Jewish communities worldwide seeking collective refuge, cultural preservation, and political self-determination. Under this reading, they are the primary beneficiary of a national framework that provides institutional sovereignty, collective voice in international affairs, and security guarantees rooted in majority control of a territorial state. Their condition as permanent minorities in diaspora contexts creates a coordination problem: no single host nation can guarantee their permanent equal participation, and historical experience (pogroms, expulsions, the Holocaust) justifies seeking an institutional arrangement where Jewish people hold state power.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora, beneficiary,
    organized, generational, constrained, global).

% The institutional actors (Zionist organizations, Israeli state apparatus, political movements) who have worked to establish and maintain Jewish sovereignty. Under this reading they set the coordination framework itself: defining what Jewish self-determination means institutionally, claiming legal and historical title to territory, and organizing the transfer of diaspora population to the sovereign state. They possess the authority to articulate the claim and the power to execute institutional arrangements.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_sovereignty_seekers, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, jewish_sovereignty_seekers, agenda_setter).

% Under the liberal nationalist reading's own logic (not the settler colonial reading), Palestinians are also a people with equal claim to self-determination via partition. The constraint as authored here ASSUMES partition into two states solves the competing national claims — makes both Jewish and Palestinian self-determination possible. They are listed as beneficiary because the reading's internal logic does not treat partition as zero-sum extraction; it treats it as mutual coordination around competing but equal national claims. The reading's coherence depends on this assumption holding.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinians_under_partition_theory, beneficiary,
    organized, generational, constrained, regional).

% The framework of nation-states, collective self-determination, and minority rights that emerged from 19th-century liberal nationalism. The constraint vindicates core premises of this order: that peoples (defined by shared identity, history, language, or culture) have equal rights to self-governance, and that territorial sovereignty is the recognized mechanism for exercising that right. The reading treats Jewish national self-determination as consistent with and supportive of liberal internationalism, not a deviation from it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_liberal_order, beneficiary,
    institutional, generational, analytical, global).

% Jewish communities and intellectuals who argue that Jewish survival and flourishing are best secured through diaspora pluralism, minority rights frameworks, and integration rather than territorial sovereignty. They are structurally excluded from the liberal nationalist frame because they reject its core premise (that national self-determination requires statehood) and propose an alternative solution to the coordination problem (pluralist rights rather than majority control). Their absence from the constraint story is significant: the reading does not engage their objection that statehood-seeking may be a misdirected response to diaspora insecurity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, diasporist_jewish_communities, excluded,
    moderate, generational, constrained, global).

% Advocates of the indigenous return reading, who claim Jewish indigeneity to the land based on unbroken historical and religious connection. They are excluded because this reading brackets the indigeneity question entirely; it grounds the claim in universal liberal nationalist principles (peoples have equal rights to self-determination) rather than particularist claims about primordial connection or historical priority. The two readings propose different grounds for the same conclusion.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, indigenous_return_theorists, excluded,
    moderate, generational, constrained, global).

% United Nations bodies, international law scholars, and comparative politics analysts who evaluate whether the claim to national self-determination accords with international law, postcolonial theory, and precedent set by other peoples' claims. They observe how the liberal nationalist reading's terms (universal principle, equal claim) compare to actual implementation (partition feasibility, recognition of Palestinian equal claim), and whether the reading's promise of mutual coordination holds or reverts to asymmetric extraction.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, jewish_sovereignty_seekers).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of diaspora insecurity and minority status: Jewish communities worldwide face persistent risk from majoritarian indifference or hostility in host nations. A shared sovereign state provides institutional mechanisms (law, military, immigration rights, international representation) guaranteeing Jewish people collective voice and political power proportional to their numbers, eliminating the vulnerability of permanent minority status. This reading treats sovereignty as the solution to the minority problem.
% TRANSFER_FUNCTION: Moves political authority and territorial control from multinational empires and host states to a Jewish-majority state, and moves (or attracts) population from diaspora communities to the sovereign territory. The constraint institutionalizes a claim: that Jewish people, like other peoples, deserve a state where they hold majority power and set the rules. The transfer is of legitimacy and territory — the reading presents this as a fair allocation in a system where each people governs itself.
% ABSENT_VOICES: Diasporist and autonomist Jewish intellectuals who argue the diagnosis is wrong (diaspora insecurity is not structural but contingent on rights frameworks and can be fixed without statehood) and the cure is worse than the disease (statehood entangles Jewish fate with a militarized state that must defend borders and manage occupation). Palestinian populations whose equal claim to self-determination is THEORETICALLY included in the reading but practically contested — the reading brackets whether partition actually secures mutual coordination or produces subordination. Voices from colonized peoples questioning whether national self-determination via a European-model nation-state is the right framework for non-European populations (postcolonial critique).
% DISAPPEARANCE_RATIONALE: If the liberal nationalist claim to Jewish self-determination vanished — if international consensus reverted to the 19th-century imperial frame where Jewish people remained a diaspora minority without institutional sovereignty — the geopolitical, legal, and immigration landscape would reorganize. Existing Israeli institutions would lose their legitimacy claim; diaspora communities would lose the option of refuge/return; international law would revert to a nation-state system where Jewish people have no guaranteed seat. The coordination problem (how does a diaspora minority secure equal standing?) would persist unsolved, and the institutional arrangement that purports to solve it would cease to exist.
% FOUNDING_PROBLEM: The founding problem is the historical vulnerability of diaspora Jews to majoritarian violence, expulsion, and legal disability in host nations — the absence of a political arrangement that guarantees Jewish people collective power and self-governance. The 19th and 20th century diagnosis: Jewish people, like other peoples, should have a state where they hold majority power and write the laws, eliminating dependence on the goodwill of host nations.
% FOUNDING_PROBLEM_CORROBORATION: The liberal nationalist reading's own tradition (Zionist political theorists, Israeli historians, liberal internationalists) attests the problem is live: diaspora vulnerability is structural to minority status. Critics outside the benefiting parties (postcolonial theorists, diasporist scholars, some international law experts) contest the problem's scope: they argue diaspora insecurity is not inherent but contingent on rights frameworks, and that statehood is a misdirected response. Palestinian advocates and indigenous return theorists attest the problem exists but propose different solutions (partition with Palestinian sovereignty, or indigenous claims that supersede the liberal nationalist frame). There is NO corroboration from outside all contesting parties — the problem statement itself is part of the kernel contest.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45 at interval end) is moderate because the reading's internal logic treats partition as mutual coordination, not extraction. Both Jewish and Palestinian peoples get self-determination; neither is theoretically victimized. However, extractiveness rises over the interval (from 0.32 to 0.47 then drops to 0.45) because actual implementation increasingly diverges from the reading's partition assumption: occupation, settlements, and security operations accumulate, transforming the constraint from mutual coordination into asymmetric control. By t=32, the extractiveness peaks (0.47) when occupation is deepest; by t=40 it modestly declines (0.45) as international pressure mounts and Palestinian state institutions develop. Suppression (0.38 at interval end) is moderate-low because the reading itself does not rely on coercion to hold — it claims universal principle. However, suppression rises over the interval (0.25 to 0.39) as the gap between the reading's claim (partition = mutual coordination) and actual practice (asymmetric occupation) widens: suppressing the settler-colonial reading's claim requires increasing institutional and rhetorical effort. Theater (0.22 at interval end) reflects the gap between the claim's justificatory language (universal liberal principle) and the enforcement reality (security operations, settlement expansion, majority control). Theater rises early (0.08 to 0.22) as occupation expands and the coordination story becomes harder to maintain with straight face; it plateaus (0.22-0.25) as the theater itself becomes institutionalized; it slightly declines (0.22) as Palestinian state institutions emerge and the fiction of mutual coordination becomes untenable even rhetorically. The time grid is shared across all metrics: every metric is authored at every time point (t=0,8,16,24,32,40), enabling the compiler to detect metric divergence and the engine to compute Type transitions.
 *
 * PERSPECTIVAL GAP:
 *   A profound perspectival gap should emerge between the liberal nationalist seat and the settler-colonial seat (the sibling reading). From the liberal nationalist position, the constraint is a fair principle: peoples have equal rights, partition solves the problem, both sides get sovereignty. From the settler-colonial position, the same institutional arrangement is colonization: dispossession via legal and military force, asymmetric outcomes, subordination of Palestinians despite the partition promise. The engine should compute these seats as experiencing different types from the same structural data because the readings have different ε values: the liberal nationalist reading's ε is low-to-moderate (assumes mutual partition); the settler-colonial reading's ε is high (asymmetric dispossession). They are different constraints with different referents. A Palestinian seat within the liberal nationalist reading should also diverge: the reading claims Palestinians are co-beneficiaries of partition, but if actual implementation subordinates Palestinian self-determination, that seat experiences the constraint as extraction, not coordination. The authored metrics (extractiveness rising, theater rising, suppression rising over time) capture the constraint's actual drift toward asymmetry, which the liberal nationalist reading does not account for in its theoretical self-justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The liberal nationalist reading authors no victims in principle — it assumes partition creates mutual coordination. However, the structural data reveals directionality tension: (1) jewish_sovereignty_seekers hold high power (institutional), arbitrage exit, and set the agenda — they drive the claim and execute the institutional arrangement. Their directionality toward the constraint is beneficiary (d near 0.1-0.2: they collect sovereignty, face no exit barrier, control the frame). (2) jewish_diaspora hold organized power, constrained exit (migration to the sovereign state is their primary option), and benefit from the coordination the state claims to provide. Their directionality is mixed beneficiary-payer (d near 0.3-0.4: they benefit from refuge/representation but remain subject to the state's decisions). (3) palestinians_under_partition_theory are theoretically equal co-beneficiaries (d near 0.5 under the reading's own logic), but actual implementation shows subordination — if occupation, settlement, and asymmetric power are the fact, their real directionality is target (d near 0.7-0.8). This mismatch between the reading's promise (d=0.5 for Palestinians) and the structural reality (d trending toward 0.8) is the engine's job to surface. No override is needed; the structural data (suppression rising, theater rising, extraction rising) already encode this divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora insecurity and minority vulnerability) is CONTESTED in status. The liberal nationalist reading attests it is live: diaspora Jews remain vulnerable minorities in most host nations. Diasporist critics attest the problem is over-diagnosed (insecurity is contingent on rights frameworks, not structural) and the cure (statehood) is worse than the disease (it creates a new security problem: defending borders and managing occupation). The disappearance verdict is world_rearranges (if the claim to Jewish self-determination vanished, the geopolitical arrangement would reorganize). The mandate mismatch is this: if the founding problem is DEAD (diaspora insecurity is solved by integration and minority rights in liberal democracies) but the disappearance verdict is REARRANGES (the state remains central to the geopolitical order), then the state persists not to solve the founding problem but to serve other functions — resource control, security maximization, institutional power. The constraint would exhibit mandatrophy: the founding justification is obsolete but the arrangement persists. The measurement series hint at this: extractiveness and suppression rise AFTER the state is established (t>16), suggesting the constraint's function has shifted from solving diaspora insecurity (early period) to maintaining institutional dominance (later period). The theater ratio rising faster than extractiveness at mid-interval (t=8-16) suggests the gap between justification and operation is widening — the reading's claim to mutual coordination is increasingly performative as asymmetric outcomes accumulate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_feasibility_assumption,
    'Does partition into two sovereign states actually constitute mutual coordination that solves both Jewish and Palestinian claims to self-determination, or does it produce asymmetric outcomes favoring Jewish state power?',
    'Empirical observation of the actual territorial, legal, and power distribution under partition; comparison to stated principles of equal claim and mutual benefit; examination of Palestinian institutional capacity to exercise self-determination without asymmetric constraints.',
    'If partition reliably produces equal outcome, the constraint remains rope (mutual coordination). If partition produces systematic asymmetry (occupation, settlement, security subordination), the constraint reclassifies to tangled_rope or snare from a Palestinian seat — the reading''s promise is violated and the structural claim becomes undefensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility_assumption, empirical, 'Whether partition assumption holds under implementation.').

omega_variable(
    diaspora_insecurity_structural_vs_contingent,
    'Is diaspora Jewish insecurity a structural feature of minority status (as the liberal nationalist reading claims) or a contingent outcome of inadequate rights frameworks and majoritarian prejudice (as diasporist critics claim)?',
    'Comparative historical analysis of diaspora Jewish security and legal status in liberal democracies with strong minority-rights institutions (post-WWII Nordic countries, contemporary Canada, Germany) vs. authoritarian states and weak-rights contexts. If strong rights frameworks eliminate persecution-risk independent of statehood, the problem is contingent.',
    'If structural, the liberal nationalist reading''s diagnosis is correct and statehood is a rational solution. If contingent, diasporism is correct and the reading misdirects the remedy toward institutional power rather than legal protections — reclassifying the constraint from rope to snare (pursuing state power to fix a rights problem).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_insecurity_structural_vs_contingent, conceptual, 'Whether diaspora insecurity is structural to minority status or contingent on institutional context.').

omega_variable(
    universal_principle_vs_particularist_ground,
    'Is the liberal nationalist reading''s claim (that Jewish people have an equal right to self-determination like other peoples) grounded in a genuinely universal principle, or does it rest on hidden particularist claims (historical connection, religious significance, or strategic interest)?',
    'Logical consistency check: does the reading apply the same principle to other diaspora peoples (Armenians, Palestinians, Kurds, Indigenous groups)? Does it extend the principle to peoples whose historical, religious, or strategic claims differ? If the principle is selectively applied, the reading is particularist, not universal.',
    'If genuinely universal, the reading is a clear instantiation of liberal nationalism principles. If particularist, it is a special pleading masked by universal language — the constraint reclassifies as snare (power-seeking rationalized as principle) with higher extractiveness and lower accessibility_collapse (the alternative — other peoples claiming the same principle — is actively suppressed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_principle_vs_particularist_ground, conceptual, 'Whether the liberal nationalist principle is genuinely universal or selectively applied.').

omega_variable(
    kernel_reading_vs_factual_claim,
    'Is the liberal nationalist claim fundamentally a reading of a contested kernel (a commitment whose legitimacy is disputed), or is it a straightforward empirical and normative claim about peoples'' rights?',
    'Examination of whether parties agree on the kernel (what Jewish self-determination means, what it requires, what its limits are) and disagree on reading (how to interpret and implement it), vs. disagreement on whether the claim is true or valid at all. If parties contest the kernel itself (what is a people, what is self-determination, what legitimates territorial claims), the reading frame applies; if they contest the normative claim (whether self-determination is good, whether it justifies statehood), the reading frame may overconstrain.',
    'If a genuine kernel reading, the constraint''s legitimacy is questioned but its referent is stable — the liberal nationalist reading is one way of reading the kernel, others are live. If not a kernel reading but a factual/normative claim, the reading frame is misapplied, and the constraint should be authored in a non-committer mode, accepting the challenge to the principle itself rather than bracketing it as reading-variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_factual_claim, conceptual, 'Whether the constraint is a kernel reading or a direct contested empirical-normative claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(jewi_tr_t0, projected).
narrative_ontology:measurement(jewi_tr_t8, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t8, projected).
narrative_ontology:measurement(jewi_tr_t16, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t16, observed).
narrative_ontology:measurement(jewi_tr_t24, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t24, observed).
narrative_ontology:measurement(jewi_tr_t32, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t32, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(jewi_be_t0, projected).
narrative_ontology:measurement(jewi_be_t8, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(jewi_be_t8, projected).
narrative_ontology:measurement(jewi_be_t16, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(jewi_be_t16, observed).
narrative_ontology:measurement(jewi_be_t24, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement_basis(jewi_be_t24, observed).
narrative_ontology:measurement(jewi_be_t32, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 32, 0.47).
narrative_ontology:measurement_basis(jewi_be_t32, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(jewi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(jewi_su_t0, projected).
narrative_ontology:measurement(jewi_su_t8, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement_basis(jewi_su_t8, projected).
narrative_ontology:measurement(jewi_su_t16, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement_basis(jewi_su_t16, observed).
narrative_ontology:measurement(jewi_su_t24, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement_basis(jewi_su_t24, observed).
narrative_ontology:measurement(jewi_su_t32, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 32, 0.39).
narrative_ontology:measurement_basis(jewi_su_t32, observed).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(jewi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__liberal_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family decomposing the contested claim 'jewish_self_determination.' The liberal nationalist reading grounds the claim in universal liberal principle (peoples have equal rights to self-determination); the settler colonial reading treats the same institutional arrangement as colonization and dispossession; the diasporist reading rejects the premise (that statehood is necessary for Jewish security); the indigenous return reading grounds the claim in historical primacy rather than universal principle; the religious covenant reading grounds it in divine obligation. Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different type. The liberal nationalist reading assumes partition solves the coordination problem (rope, low-moderate ε); the settler colonial reading treats the same arrangement as asymmetric extraction (snare or tangled_rope, high ε). The family structure enables measurement of how the readings diverge under implementation pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
