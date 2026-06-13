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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish National Self-Determination (Liberal Nationalist Reading)
 *   domain: political/philosophical
 *
 * SUMMARY:
 *   The liberal nationalist reading of Jewish self-determination asserts that
 *   Jewish people constitute a nation with equal standing to other peoples in
 *   claiming the right to territorial self-governance. This reading grounds
 *   legitimacy in the principle of universal national self-determination—a
 *   principle that liberal political philosophy extends to all peoples, not
 *   excepting Jews. The constraint is the institutional and normative
 *   machinery that sustains this claim: international legal recognitions,
 *   diplomatic framing, institutional alliances that treat the Jewish
 *   national claim as legitimate within a framework of equal national
 *   standing. The reading explicitly brackets religious-covenant framings
 *   (delegitimized as theological rather than political) and diasporist
 *   alternatives (treated as politically suboptimal given historical
 *   persecution). It engages the settler-colonial and indigenous-return
 *   readings as competing territorial claims requiring negotiated resolution,
 *   not as delegitimations of the Jewish national principle itself.
 *
 * KEY AGENTS:
 *   - Jewish diaspora and persecuted minorities: seek refuge and institutional protection through recognized national sovereignty
 *   - Palestinian national movement: asserts competing legitimate claim to territorial self-determination
 *   - Liberal democratic institutional actors: set the agenda by universalizing the principle of equal national self-determination
 *   - Regional territorial neighbors: bear externality costs of the claim's political instantiation
 *   - Diasporist and postcolonial critics: excluded from the framework's design assumptions but contest its premises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.38).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.42).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish National Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political/philosophical").

domain_priors:requires_active_enforcement(jewish_self_determination__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'f78709f6-dccb-4f70-848b-9ef2bc8a91e3').
narrative_ontology:cs_kernel_codification('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', fixed_text).
narrative_ontology:cs_authority_grounding('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', lineage).
narrative_ontology:cs_interpretation_layer_present('f78709f6-dccb-4f70-848b-9ef2bc8a91e3').
narrative_ontology:cs_reading_relation('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', foundational, jewish_people_constitute_nation).
narrative_ontology:cs_axiom_status(jewish_people_constitute_nation, holdable).
narrative_ontology:cs_axiom_grounding('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', jewish_people_constitute_nation, deontological).
narrative_ontology:cs_axiom('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', foundational, self_determination_principle_universal).
narrative_ontology:cs_axiom_status(self_determination_principle_universal, holdable).
narrative_ontology:cs_axiom_grounding('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', self_determination_principle_universal, deontological).
narrative_ontology:cs_axiom('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', secondary, national_claims_resolved_via_partition).
narrative_ontology:cs_axiom_status(national_claims_resolved_via_partition, holdable).
narrative_ontology:cs_axiom_grounding('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', national_claims_resolved_via_partition, instrumental).
narrative_ontology:cs_reference_frame('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', universal_national_self_determination_framework).
narrative_ontology:cs_drift_state('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', contemporary_post_partition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f78709f6-dccb-4f70-848b-9ef2bc8a91e3', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, persecuted_jewish_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, regional_territorial_neighbors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed Jewish communities across multiple nations facing recurrent antisemitism, legal disability, and persecution. Benefit from recognition as a nation with legitimate claim to self-determination: this recognition provides institutional leverage in host-state negotiations, provides refuge alternative if persecution escalates, and elevates their status from permanent minority (tolerating host-state mercy) to people with equal standing (claiming rights). Exit from this frame would mean abandoning sovereignty pursuit and accepting minority status; historically, that choice has not proven stable given persecution recurrence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty, beneficiary,
    organized, generational, constrained, global).

% Communities experiencing active violence, legal exclusion, and systemic persecution in specific host states. For them, the national self-determination claim functions as a material refuge guarantee—a state where they would be majority and thus structurally protected from minority-status vulnerability. They cannot exit the frame; they depend on its instantiation for survival.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, persecuted_jewish_minorities, beneficiary,
    powerless, immediate, trapped, regional).

% Diplomats, lawyers, philosophers, and institutional actors advancing the principle that all peoples have equal self-determination rights. They set the agenda by institutionalizing the principle in international law (UN Charter, UNESCO declarations), by extending recognition to Jewish national claims, by treating the question as one of universal principle rather than exception-making. They benefit from consistency in principle application and from the legitimacy the principle confers on liberal universalist frameworks.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_institutional_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Asserts competing legitimate claim to territorial self-determination in the same land. The reading frames Palestinian resistance as an expression of equally valid national self-determination (not illegitimate opposition to legitimate Jewish claims), requiring negotiated boundary resolution. Palestinians bear the cost of the reading: their claims are recognized as legitimate in principle but are systematically subordinated in institutional practice (Jewish state exists; Palestinian state does not as of writing). The reading treats this as a problem of implementation, not principle; Palestinians experience it as having less institutional leverage despite equal formal standing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, payer,
    organized, generational, constrained, regional).

% States bordering the claimed territory and their populations. The reading's instantiation produces changed regional political configurations, security costs (if state is militarized against neighbors), and resource competition. Neighbors benefit from stable bilateral relations with a recognized state (rather than disputed territory) but bear externality costs of the initial territorial claim's enforcement. They cannot easily exit; geography constrains their options.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, regional_territorial_neighbors, payer,
    institutional, generational, constrained, regional).

% Jewish thinkers and communities arguing that diaspora pluralism, not territorial nationalism, is the authentic Jewish path and superior political strategy. They argue that Jewish strength historically lay in adaptive minority flourishing, that nationalism imports European Christian categories inappropriate to Jewish tradition, and that the territorial solution is disproportionate to the problem and destructive to actual diaspora communities. The reading systematically excludes them from institutional legitimacy discourse by treating nationalism as the only viable framework; they are identity-locked (cannot exit Jewish community) but locked out of the principle-setting conversation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, diasporist_jewish_intellectuals, excluded,
    moderate, generational, identity_locked, global).

% Scholars and advocates analyzing the constraint as a settler-colonial project rather than a legitimate national claim. They argue the reading uses Western universalist liberal nationalism to mask and rationalize systematic displacement of indigenous Palestinians. They are excluded because the reading brackets settler-colonial analysis as empirically or normatively misguided, not as an alternative legitimate reading of the same kernel. They contribute to resistance (0.71) by offering systematic delegitimating counter-analysis.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, postcolonial_critics, excluded,
    organized, generational, analytical, global).

% Jewish and Christian thinkers grounding the territorial claim in divine covenant rather than secular national principle. They argue the reading's secular framing is a category error—that the claim's true warrant is theological, not political. The reading excludes them by treating religious grounding as irrelevant to legitimate political claims, implicitly placing religious authority outside the scope of legitimate democratic discourse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, religious_covenant_advocates, excluded,
    moderate, civilizational, identity_locked, global).

% Other diaspora and indigenous populations (Kurds, Rohingya, Armenians, others) also claiming self-determination but facing comparable or greater institutional obstruction. The liberal nationalist reading grants them equal standing in principle but offers no mechanism for resolving simultaneous, overlapping territorial claims. They are excluded from the particular negotiating table when Jewish self-determination is instantiated; the principle that warrants Jewish claims also warrants theirs, but resources, political will, and geopolitical priority ensure only some claims get recognized.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, indigenous_populations_other_territories, excluded,
    powerless, generational, trapped, global).

% UN bodies, state sovereignty regimes, human rights courts. They observe the constraint as a test case for the consistency and universality of liberal nationalist principle. Do they extend equal recognition to all national claims, or does the principle contain selective exceptions? They do not collect or pay directly but adjudicate institutional consistency.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_law_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, liberal_institutional_advocates).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a normative framework recognizing diaspora Jewish communities as a people with legitimate national standing equivalent to other peoples. The coordination problem: how do persecuted, dispersed populations gain institutionalized protection and equality without supplicating to host-state tolerance? The solution: establish the principle that all peoples have equal self-determination rights and extend it to include Jewish people. This addresses the asymmetry where some peoples (Italians, Greeks, Poles) gained recognized states while Jewish people were denied the same claim. Solves the coordination problem of bringing Jewish national aspirations within the same universal principle framework.
% TRANSFER_FUNCTION: Moves territorial authority and governance legitimacy from undefined or host-state-controlled jurisdictional status to Jewish-majority collective self-determination. Transfers recognition (Jews treated as a nation, not a religious minority requesting accommodation within host states). Transfers institutional leverage (Jewish communities gain standing in international negotiations as a people, not as interest groups within other nations). The arrangement moves decision-making power about Jewish communities from external host-state actors to Jewish internal institutions. It also transfers resource costs to competing territorial claimants and neighboring states, and transfers legitimacy burden to those who would contest the principle (they must argue against universalized national self-determination, not just against Jewish claims).
% ABSENT_VOICES: Diasporist Jewish intellectuals and communities who reject nationalism as inappropriate to Jewish history and identity. Postcolonial critics who read the claim as settler-colonial displacement rationalized through universal principle language. Religious covenant readers who treat the secular political framing as a category error. Palestinian populations and their supporters who would prioritize historical continuity and demographic presence over ancient territorial connection. Indigenous populations other than Palestinian (Kurds, Rohingya, others) who claim equal self-determination but are institutionally sidelined. Secular liberal critics who argue the principle of universalized nationalism produces unresolvable overlapping claims and should be replaced with minority-rights frameworks.
% DISAPPEARANCE_RATIONALE: If the principle of Jewish national self-determination disappeared, diaspora governance strategies would shift from sovereignty-seeking to minority-rights advocacy and diaspora pluralism frameworks. International law would lose a stated precedent for recognizing dispersed peoples as nations deserving sovereign territory. Palestinian claims would face a different negotiating partner—instead of two peoples with equal self-determination claims (requiring partition), the framework would be Palestinian indigenous/historical presence versus Jewish minority-rights claims (a different structural position). Regional security configurations would reorganize around different authority structures (whether multinational, federal, or religious-autonomous arrangements). The world would rearrange toward diaspora plural governance or secular-nationalist denial of the principle to all national claims.
% FOUNDING_PROBLEM: Jewish communities in medieval and early-modern Europe were subjected to systematic legal disability, violent persecution, and institutionalized vulnerability rooted in their status as permanent, unassimilable minorities in Christian host states. Recurring pogroms, expulsions, ghettoization, and forced conversions created conditions of chronic existential insecurity. The founding problem: a dispersed people faces extinction or permanent subordination when their survival depends on host-state tolerance that is historically unreliable and recurrently revoked by persecution.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholars document medieval and early-modern persecution (confirming the founding problem existed). Jewish sovereignty advocates and liberal institutional actors attest the problem is live and ongoing (citing contemporary antisemitism, recurrent violence, diaspora insecurity). Diasporist advocates and human rights scholars contest that the founding problem has been substantially addressed through minority-rights frameworks, universal human rights law, and liberal democratic citizenship protections—making the territorial sovereignty response structurally disproportionate to the current problem. International law specialists note that the principle of self-determination was universalized (UN Charter 1945) AFTER the founding persecution problem's acute phase, suggesting the principle may have been retroactively applied to justify a solution already politically decided, rather than deduced from the problem itself.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at interval end) because the reading coordinates a genuine problem—protecting diaspora populations through recognized national standing—but does so by imposing costs on competing claimants (Palestinians, regional states) without compensating mechanisms inherent to the principle itself. The principle says 'equal self-determination for all peoples' but cannot resolve simultaneous overlapping territorial claims; that gap is where extraction emerges—the principle beneficiaries (Jewish sovereignty advocates) gain institutional leverage while those bearing the cost of its instantiation (Palestinian displaced persons, neighboring jurisdictions) carry the burden. Suppression is moderate (0.42) because the reading must actively defend itself against delegitimizing counter-readings (settler-colonial, religious-covenant, diasporist) and exclude those counter-readings from institutional space. Theater ratio rises over time (0.15→0.28) as institutional performance of the principle increases relative to its coordination function—more diplomatic emphasis on the principle itself, less on negotiating specific boundaries. Accessibility collapse is moderate (0.65) because alternatives to territorial sovereignty remain institutionally visible (minority-rights frameworks, diaspora pluralism, religious autonomy) but are systematically delegitimized within liberal nationalist institutional discourse.
 *
 * PERSPECTIVAL GAP:
 *   The reading's fundamental asymmetry: it proposes a universal principle (all peoples have equal self-determination rights) but cannot institutionalize it universally (simultaneous overlapping territorial claims cannot all be satisfied equally). The gap between principle and practice is managed by prioritizing one claim's instantiation (Jewish), treating competing claims as negotiating variables rather than equally binding, and delegitimizing alternative frameworks (diasporist, religious, pluralist) that might distribute the principle differently. This gap is not a defect in the reading; it is structural to any nationalist principle applied to overlapping territorial claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora seeking sovereignty sits near full beneficiary (d ≈ 0.25): they gain institutional recognition, refugee protection, and majority governance without bearing the territorial-conflict costs directly (those fall on neighbors and Palestinians). Palestinian national movement sits near target (d ≈ 0.75): the principle of self-determination is universal, but its instantiation as Jewish-majority state requires negotiated partition that absorbs Palestinian territorial claims into minority or displaced status. Regional neighbors sit between (d ≈ 0.60): they participate in the coordination function (neighbor states recognize sovereignty, benefit from stable bilateral relations) but bear security and resource externalities. Diasporist Jewish critics sit near full target (d ≈ 0.85) from within the framework: the reading suppresses their alternative and imposes diaspora assimilation pressures. Liberal democratic institutional advocates sit near full beneficiary (d ≈ 0.15): they sustain the principle and defend its applicability without bearing instantiation costs. Alternative diaspora movements sit outside (d undefined): they are excluded from the negotiating frame entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish persecution and insecurity in host states) is historically documented and remains contested in its contemporary prevalence. The reading's solution (territorial national sovereignty) has been partially instantiated; the mandate to provide sanctuary and security is live but contested. Some anti-semitism persists; some diaspora populations are secure. The reading avoids mandatrophy by tying itself not to the original problem's full resolution but to the principle of equal self-determination—a principle that has become independent of the founding persecution context and is now defended on universal grounds. However, tension remains: if the founding problem (persecution) has been substantially addressed through minority-rights frameworks and human rights law (as diasporist and postcolonial readers argue), then the continued assertion of national sovereignty becomes harder to justify within the liberal framework itself (it violates the principle of proportionality to founding problem). This tension is captured in the contested founding_problem_status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_feasibility_and_mutual_recognition,
    'Can competing national claims (Jewish and Palestinian) be resolved through negotiated territorial partition where both peoples gain institutionalized self-determination?',
    'Historical negotiations (Camp David, Oslo, Geneva Initiative); contemporary feasibility assessments; empirical outcomes of similar partition agreements (India-Pakistan, Ireland, Korea) and their success/failure rates.',
    'If partition is feasible and both sides achieve recognized statehood, the reading is validated as genuine coordination of competing legitimate claims. If partition repeatedly fails or one side is systematically excluded, the reading slides toward tangled extraction (one party''s self-determination institutionalized, the other''s suppressed). If partition is structurally impossible (overlapping territorial maximalist claims), the reading becomes incoherent and the constraint collapses toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_feasibility_and_mutual_recognition, empirical, 'Whether the liberal nationalist principle can be institutionalized for both competing claimants simultaneously.').

omega_variable(
    universal_vs_particularist_self_determination,
    'Is the right to national self-determination genuinely universal (available equally to all peoples, including Jews), or is its application selective and historically contingent (determined by power, international recognition, historical timing)?',
    'Comparative analysis of how the principle was applied to other peoples (Italians, Greeks, Poles gained recognition; Kurds, Baloch, Rohingya do not); institutional examination of recognition criteria; study of how precedent was set.',
    'If truly universal, the reading validates the principle and focuses on implementation. If selective, the reading is either complicit in selective application (making it extractive for excluded groups) or the principle itself is delegitimized, and the reading becomes a post-hoc rationalization of geopolitical power decisions rather than principled self-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_particularist_self_determination, conceptual, 'Whether the constraint instantiates a universal principle or a particularist exception.').

omega_variable(
    territorial_vs_cultural_national_claims,
    'Must national self-determination require territorial sovereignty, or can it be satisfied through cultural autonomy, minority-rights protections, and diaspora pluralism?',
    'Historical comparison of diaspora populations that achieved flourishing without territorial sovereignty (medieval Jewish communities in some Islamic contexts, contemporary diaspora Armenians, liberal democracies'' minority-rights frameworks); study of whether territorial and non-territorial self-determination are structurally equivalent.',
    'If national self-determination is decoupled from territorial requirement, diasporist reading becomes a viable alternative, suppression of diasporist positions is unjustifiable, the reading becomes one option among several rather than the principal framework, and extractiveness potentially decreases (alternative paths reduce zero-sum conflict with other territorial claimants). If territorial sovereignty is necessary, the reading is reinforced and partition becomes non-negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_vs_cultural_national_claims, conceptual, 'Whether the constraint''s core commitment is to national self-determination generically or specifically to territorial instantiation.').

omega_variable(
    jewish_national_identity_essentialism,
    'Is Jewish collective identity best understood as a stable, trans-historical nation (warranting self-determination as other nations claim it), or as a fluid, contextually-constructed identity whose political expression should remain diverse and non-exclusive?',
    'Historical analysis of Jewish identity formations across time and place; study of how identity claims are made and contested within Jewish communities; philosophical analysis of what constitutes ''a people'' for self-determination purposes.',
    'If Jewish identity is essentialist and historically continuous, the reading is grounded in fact and the claim is legitimate. If Jewish identity is contingent and contextually constructed, the reading reifies a particular construction as natural, and the universalist principle it invokes becomes a cover for a particular identity politics. This divergence is the key omega for the false-summit domain: the reading claims natural national fact while constructing it through institutional and discursive means.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jewish_national_identity_essentialism, conceptual, 'Whether Jewish nationhood is an essential fact or a constructed claim that benefits from appearing essential.').

omega_variable(
    indigenous_vs_historical_claim_hierarchy,
    'When multiple peoples claim the same territory through different historical narratives (Jewish connection dating to antiquity vs. Palestinian presence across centuries), which temporal claim supersedes the other for self-determination purposes, and on what grounds?',
    'Analysis of international law precedent on indigenous vs. historical claims (does longest continuous presence win, most recent territorial control, current demographic majority, or some other principle?); comparative case study of how overlapping historical claims are adjudicated elsewhere.',
    'If ancient territorial connection counts equally to recent presence, the indigenous-return reading has standing equal to Palestinian claims based on contemporary occupation. If recent presence and continuous habitation take priority, Palestinian claims supersede ancient Jewish claims, and the reading must explicitly defend prioritizing discontinuous historical connection over continuous demographic presence—a defense that requires moving outside liberal universalist logic into particularist Jewish historical narratives. The resolution determines whether the reading''s universalism is actually coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_vs_historical_claim_hierarchy, conceptual, 'The hierarchy of historical claims in self-determination frameworks and which narrative temporality is privileged by the liberal universalist reading.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'The measured suppression (0.42) reflects institutional exclusion of diasporist, religious-covenant, and settler-colonial readings from legitimacy discourse. Is this suppression structurally enforced (institutions actively police discourse and exclude alternative framings), or internalized (Jewish communities have absorbed the nationalist narrative as self-evident truth)?',
    'Post-departure analysis: examination of diaspora contexts where the liberal nationalist reading has NOT achieved institutional hegemony (Jewish communities in diaspora countries not aligned with sovereignty state); study of how readily alternative framings re-emerge when institutional suppression is lifted.',
    'If suppression is primarily structural (institutional police), removing enforcement infrastructure could allow alternatives to resurface. If suppression is primarily internalized (nationalist narrative absorbed as natural), the constraint persists beyond institutional enforcement, and exit from the frame requires identity-reconstruction at the community level. Mixed structural and internalized suppression suggests the constraint is self-sustaining and more extractive than the institutional suppression metric alone captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether measured suppression reflects active institutional policing or internalized narrative adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t10, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(jewi_tr_t30, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement_basis(jewi_be_t10, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(jewi_be_t30, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(jewi_be_t40, observed).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(jewi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(jewi_su_t10, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(jewi_su_t30, observed).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(jewi_su_t40, observed).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(jewi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__liberal_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, palestinian_self_determination__competing_national_claim).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, universal_national_self_determination_principle).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel admits five structurally distinct readings differing in (1) foundational legitimacy grounding (liberal universalism vs. religious covenant vs. indigenous historical right vs. European settler colonialism vs. diaspora pluralism), (2) beneficiary structure (who secures what through the constraint), and (3) epsilon values (whether the claim is coordination solving a genuine problem or extraction imposing costs on competing claimants). This reading (liberal_nationalist_reading) treats the constraint as genuine coordination solving the problem of diaspora protection through universalized principle. The settler_colonial_reading treats it as pure extraction masking colonial displacement. The indigenous_return_reading treats it as decolonial reclamation. The diasporist_reading treats it as inappropriate and damaging to actually-existing diaspora communities. The religious_covenant_reading treats the whole secular-political framing as a category error. Each reading produces a different constraint story with different epsilon values, different beneficiary/victim structures, and different type classifications. All five are equally authored with independent epsilon estimates and structural data; the engine computes what the convergence or divergence between readings reveals about the kernel itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
