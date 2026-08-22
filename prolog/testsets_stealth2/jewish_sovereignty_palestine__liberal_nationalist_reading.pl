% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination and Statehood in Palestine — Liberal Nationalist Reading
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'Jewish
 *   sovereignty in Palestine': the liberal nationalist reading, under which
 *   the Jewish people possess a collective self-determination right and
 *   statehood in the ancestral homeland is a legitimate exercise of that
 *   right — legitimacy conditioned, on this reading's own terms, on
 *   reciprocal recognition of Palestinian self-determination (partition or a
 *   binational framework). The epsilon referent is the STANDING ARRANGEMENT
 *   under contest — the actual sovereignty arrangement as practiced,
 *   including the post-1967 occupation layer, settlement growth, and
 *   asymmetric legal regimes — assessed by this reading's own lights, never
 *   the partitioned/binational arrangement the reading endorses. Sibling
 *   readings (settler_colonial, religious_zionist, cultural_zionist,
 *   post_zionist) are separate constraint files, not folded into this one;
 *   committer structure is routed to omega variables. The claim/metric
 *   independence rule is observed deliberately: the reading CLAIMS
 *   tangled_rope (genuine coordination — rescue of a stateless nation —
 *   entangled with recognized asymmetric costs), while the metrics
 *   independently describe substantially extractive, actively enforced
 *   operation whose extraction accumulated sharply after 1967. Where the
 *   engine's computed verdict diverges from the claim, that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - jewish_nation_collective: Primary beneficiary (organized/identity_locked) — receives sovereign protection, ingathering, and national realization through the state
 *   - israeli_state_institutions: Agenda setter (institutional/arbitrage) — administers law, military force, land registry, and settlement policy; captures territorial jurisdiction; can reconfigure the arrangement at high cost
 *   - palestinian_self_determination_claimants: Primary target (organized/trapped) — bears territorial fragmentation, restricted movement, and asymmetric rule; organized but without sovereign leverage
 *   - palestinian_refugee_and_displaced_communities: Founding-cost bearer (powerless/trapped) — bears the arrangement's founding displacement across host states; largely outside compensation frameworks
 *   - palestinian_citizens_of_israel: Dual-positioned (moderate/constrained) — hold citizenship inside the beneficiary state while bearing identity-based planning, budgetary, and symbolic costs
 *   - diaspora_jewish_communities: Secondary beneficiary (organized/identity_locked) — supply political and financial support; receive identity anchoring and refuge guarantee
 *   - binational_confederation_advocates: Excluded voice (moderate/constrained) — propose frameworks outside the two-sovereignty conversation; marginal in both polities
 *   - international_legal_order: Analytical observer (institutional/analytical) — adjudicates occupation, settlement, and annexation legality; rulings neither party fully accepts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.66).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.64).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination and Statehood in Palestine — Liberal Nationalist Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '1ae4392c-4c88-402f-9827-20bcc74588f3').
narrative_ontology:cs_kernel_codification('1ae4392c-4c88-402f-9827-20bcc74588f3', formalized).
narrative_ontology:cs_authority_grounding('1ae4392c-4c88-402f-9827-20bcc74588f3', lineage).
narrative_ontology:cs_interpretation_layer_present('1ae4392c-4c88-402f-9827-20bcc74588f3').
narrative_ontology:cs_reading_relation('1ae4392c-4c88-402f-9827-20bcc74588f3', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ae4392c-4c88-402f-9827-20bcc74588f3', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ae4392c-4c88-402f-9827-20bcc74588f3', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ae4392c-4c88-402f-9827-20bcc74588f3', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('1ae4392c-4c88-402f-9827-20bcc74588f3', foundational, jewish_collective_self_determination_right).
narrative_ontology:cs_axiom_status(jewish_collective_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('1ae4392c-4c88-402f-9827-20bcc74588f3', jewish_collective_self_determination_right, deontological).
narrative_ontology:cs_axiom('1ae4392c-4c88-402f-9827-20bcc74588f3', foundational, legitimacy_conditional_on_reciprocal_national_rights).
narrative_ontology:cs_axiom_status(legitimacy_conditional_on_reciprocal_national_rights, holdable).
narrative_ontology:cs_axiom_grounding('1ae4392c-4c88-402f-9827-20bcc74588f3', legitimacy_conditional_on_reciprocal_national_rights, deontological).
narrative_ontology:cs_reference_frame('1ae4392c-4c88-402f-9827-20bcc74588f3', mutual_recognition_partition_frame).
narrative_ontology:cs_drift_state('1ae4392c-4c88-402f-9827-20bcc74588f3', contemporary_settlement_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ae4392c-4c88-402f-9827-20bcc74588f3', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_nation_collective).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_self_determination_claimants).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugee_and_displaced_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish people as a national collective exercising self-determination through the State of Israel. Receives sovereign protection, immigration rights, and national-cultural realization. For much of the collective the claim itself is constitutive of modern Jewish identity, so exiting the claim — even for internal critics — carries identity cost; criticism typically operates inside the frame rather than leaving it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_nation_collective, beneficiary,
    organized, generational, identity_locked, global).

% Administers the arrangement: legislature, courts, military, land registry, settlement authorities. Sets the terms of Palestinian movement, residency, construction, and political organization in the occupied territories and shapes citizenship architecture inside the Green Line. Captures territorial jurisdiction, planning control, and associated revenue. Can reconfigure the arrangement — withdraw, annex, partition — but each path carries existential-level coalition and security costs.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions, beneficiary).

% The Palestinian national collective asserting a parallel self-determination claim. Bears territorial fragmentation (non-contiguous autonomous areas), permit-regulated movement, asymmetric legal regimes under occupation, and settlement encirclement. Organized through the PLO, the Palestinian Authority, and civil society, but without sovereign leverage over borders, airspace, or water. Exit means emigration or absorption into neighboring states, both personally and politically costly.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_self_determination_claimants, payer,
    organized, generational, trapped, regional).

% Descendants of those displaced in 1948 and 1967, living in camps and exile communities across Lebanon, Syria, Jordan, the West Bank, and Gaza. Bear the arrangement's founding costs: lost property, statelessness or second-class host-state status, and blocked return. Sit outside every compensation and negotiation framework so far constructed. Coalition potential is suppressed by host-state restrictions on camp political organization and by divergent host-state interests.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugee_and_displaced_communities, payer,
    powerless, generational, trapped, continental).

% Roughly twenty percent of Israel's citizenry. Vote, hold office, and access state services; simultaneously bear differential treatment in land-use planning, municipal budgeting, admissions committees, and the symbolic hierarchy codified in the Nation-State Law. Their national symbols and kinship ties to the occupied territories are politically fraught. Exit would mean abandoning home, citizenship, and community; staying means navigating membership in a state constituted around another collective's self-determination.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel, beneficiary).

% Jewish communities outside Israel that supply political lobbying, philanthropy, and advocacy infrastructure sustaining the arrangement internationally. Receive identity anchoring (Israel as collective center) and a standing refuge guarantee. Internal dissent carries communal cost — organizations critical of state policy face funding withdrawal and social sanction — which fuses support with belonging and stabilizes the beneficiary-side subsidy beyond narrow material interest.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).

% Intellectuals, activists, and some politicians proposing frameworks outside the two-sovereignty conversation: binational state, confederation, equal-citizenship arrangements. Marginal in both polities — treated as unrealistic in Israeli discourse and as defeatist or collaborationist in parts of Palestinian discourse. Would object that the partition frame itself entrenches the extraction; they are structurally outside the room where the arrangement's terms are set.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, binational_confederation_advocates, excluded,
    moderate, generational, constrained, global).

% UN organs, the International Court of Justice, treaty bodies, and humanitarian law institutions. Adjudicate the legality of occupation practices, settlement expansion, and annexation measures; produce rulings and resolutions that neither party fully accepts and that lack independent enforcement. Record the arrangement's conduct against international-law baselines and supply the evidentiary surface other seats cite.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_legal_order, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the security-and-belonging problem of a historically stateless minority nation: concentrates protection, immigration, and national development under one sovereign roof, ending a millennium in which Jewish safety depended on host-state toleration. On this reading's own terms it also nominally solves the two-peoples-one-territory problem through partition-with-mutual-recognition logic.
% TRANSFER_FUNCTION: Moves territorial jurisdiction, land, and political primacy from the Palestinian inhabitants of the contested territory to the Jewish national collective and its state institutions; moves security guarantees, refuge rights, and identity anchoring to Jewish populations worldwide; moves the costs of displacement, restriction, and statelessness onto the Palestinian collective.
% ABSENT_VOICES: Refugee communities outside every negotiation frame; binational and confederation advocates marginal in both polities; Palestinian citizens of Israel in questions of the state's constitutive character; future generations of both collectives, who will inherit whatever territorial and legal architecture hardens now. Unanimity in favor of the arrangement arises partly because these seats were never in the room where its terms were set.
% DISAPPEARANCE_RATIONALE: If the sovereignty arrangement vanished overnight, roughly seven million residents would stand amid competing claims with no governing frame; regional states would move on the vacuum; refugee-return disputes would ignite without any instrument to adjudicate them; and the diaspora refuge guarantee would evaporate. The Middle East rearranges around the hole — this is not a constraint the world ignores.
% FOUNDING_PROBLEM: European antisemitism culminating in the Holocaust left the Jewish people a stateless minority with no sovereign protector; Zionism proposed territorial concentration and self-governance in the ancestral homeland as the remedy.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: mainstream Holocaust historiography and European state archives document the persecutory record and the statelessness problem; Palestinian and Arab historiography attests the founding problem was real while disputing the justice and costs of the chosen remedy; no serious scholarly party denies the problem existed — the live dispute is over whether this remedy, sited on inhabited land, was proportionate and whether its costs were borne by parties who never consented.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66: the standing arrangement delivers real coordination (protection, ingathering, functioning state) while transferring territorial jurisdiction and political primacy away from the Palestinian collective, with the occupation layer adding a continuously deepening extraction wedge. Suppression is 0.64 and is authored as a RAW structural property — it is NOT scaled by power or scope; only extractiveness is scaled by the engine. It reflects the enforcement machinery the arrangement requires: military administration, permit regimes, checkpoint infrastructure, and legal asymmetries that must be actively maintained against persistent objection. Theater_ratio is 0.50: state functions are real, but a growing share of activity is performative maintenance of the two-state vocabulary (negotiation rituals, recognition statements) that no longer tracks implementation — classic Goodhart drift of the peace-process proxy. Accessibility_collapse is 0.42: alternatives (binational state, confederation, federated variants, two-state revisions) remain live and articulable, unlike a natural law's total collapse of alternatives. Resistance is 0.72: two intifadas, sustained civil resistance, BDS campaigns, recurring UN majorities, and litigation — the arrangement meets real, organized, continuing resistance, which is dispositive evidence it is a construct defended by force, not a natural feature. Coalition note: the three victim seats are divided by geography, legal status, and host-state politics, which suppresses coalition power despite their aggregate size; the refugee seat's coalition potential is further constrained by host-state regimes that restrict camp political organization.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the jewish_nation_collective and diaspora seats, the arrangement presents as rope: a life-saving coordination achievement that solved statelessness and persecution — exit is identity_locked because the claim is constitutive of modern Jewish identity, so even critics cannot cheaply abandon the frame. From the palestinian_self_determination_claimants and refugee seats, the same structure presents as enforced extraction with a coordination veneer: trapped exit, generational cost-bearing, and a transfer function running against them. The palestinian_citizens_of_israel seat straddles: formal membership in the beneficiary polity with identity-indexed costs, producing a genuinely intermediate directionality. The israeli_state_institutions seat experiences the arrangement as its own accomplishment and revenue-jurisdiction base, with arbitrage-grade ability to reshape it. The engine computes these per-seat classifications from the structural data; this commentary explains the asymmetry, it does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the jewish_nation_collective and diaspora_jewish_communities (d near the beneficiary end; the diaspora slightly above zero because it pays diplomatic and financial costs of defending the arrangement). Victim declarations drive high directionality for the three Palestinian seats; trapped exit pushes the claimants and refugees toward the full-target end, while the citizens_of_israel seat sits nearer symmetric because it shares citizenship benefits. The israeli_state_institutions seat derives low d as agenda-setter and capture point. No directionality_overrides were needed: the beneficiary/victim declarations plus exit options already produce the correct ordering, and the schema's override mechanism keys on power atoms, which would misfire here since multiple distinct seats share power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — statelessness and exterminatory antisemitism — is LIVE, corroborated from outside the beneficiary set, so this is not a mandatrophy case: the arrangement has not outlived its mandate. The classification discipline cuts both ways. Against the pure-snare mislabel: the coordination function is real and historically verifiable (a persecuted minority nation acquired protection), which is why the claim is tangled_rope rather than snare. Against the pure-rope mislabel: the extraction series shows accumulation (0.12 to 0.66) with the steepest rise at 1947–1967, and theater_ratio crossing 0.45 after Oslo — the signature of rent-seeking layered onto coordination. The danger case this story flags via omega is the transition path: if the reciprocal-partition conditionality is abandoned in practice while retained rhetorically, the coordination story becomes cover and the computed type should drift toward snare. The measurement grid is designed to make that drift detectable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of kernel jewish_sovereignty_palestine (liberal_nationalist_reading). How would each sibling reading — settler_colonial_reading, religious_zionist_reading, cultural_zionist_reading, post_zionist_reading — restructure the beneficiary/victim set, epsilon, and classification?',
    'Cross-file comparison once the five sibling reading files are generated: diff each sibling''s beneficiaries, victims, epsilon, and computed type against this file''s.',
    'The settler_colonial_reading raises epsilon sharply (displacement-regime referent, beneficiaries reframed as operators of that regime); the religious_zionist_reading removes the partition conditionality, raising suppression and victim scope; the cultural_zionist_reading lowers extractiveness (no sovereignty requirement); the post_zionist_reading shifts the referent to the civic-equality deficit of the achieved state. Classification of THIS file is stable only under this reading''s own premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    reciprocal_partition_conditionality,
    'Does the standing arrangement still satisfy this reading''s own legitimacy condition — reciprocal Palestinian self-determination via partition or binational framework — or has practice abandoned that condition while retaining the rhetoric?',
    'Settlement-population and jurisdiction trajectories, final-status negotiation records, annexation legislation, and the legal architecture governing Area C versus Areas A/B.',
    'If reciprocity has been abandoned in practice, the residual coordination claim becomes cover and the computed type drifts toward snare; if a credible reciprocal framework remains reachable, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_partition_conditionality, empirical, 'Whether the reading''s own co-equal-claimant premise is honored by the arrangement it legitimates.').

omega_variable(
    self_determination_symmetry,
    'Is the self-determination principle applied symmetrically to both collectives in the arrangement''s operation, or selectively to the Jewish collective?',
    'Compare the legal instruments governing each collective''s national expression: the Nation-State Law''s exclusivity clauses, the Law of Return versus blocked Palestinian return, restrictions on Palestinian national institutions, and citizenship pathways.',
    'Demonstrated asymmetry indicates the coordination function serves one collective only, raising effective extraction for every Palestinian seat and weakening the rope component of the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_determination_symmetry, conceptual, 'Symmetry of the organizing principle across the two national collectives.').

omega_variable(
    diaspora_identity_lock_depth,
    'How deeply is diaspora support fused with communal belonging, such that internal criticism of the arrangement carries social or organizational exclusion cost?',
    'Survey data on communal sanction for dissent, organizational funding withdrawal patterns against critical Jewish organizations, and longitudinal tracking of identity-politicization after crises.',
    'Deeper identity lock sustains beneficiary-side subsidy of the arrangement beyond material interest, damping measured resistance and stabilizing enforcement supply independent of the arrangement''s performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_identity_lock_depth, empirical, 'Depth of identity fusion binding the diaspora beneficiary seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1897, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1897, 0.08).
narrative_ontology:measurement(jewi_tr_t1922, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1922, 0.12).
narrative_ontology:measurement(jewi_tr_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1947, 0.18).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.32).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(jewi_tr_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2026, 0.5).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1897, 0.12).
narrative_ontology:measurement(jewi_be_t1922, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1922, 0.28).
narrative_ontology:measurement(jewi_be_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1947, 0.52).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(jewi_be_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1897, 0.05).
narrative_ontology:measurement(jewi_su_t1922, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1922, 0.22).
narrative_ontology:measurement(jewi_su_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1947, 0.48).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.62).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.54).
narrative_ontology:measurement(jewi_su_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel 'Jewish sovereignty in Palestine'. The colloquial label covers five structurally distinct claims with different epsilon referents, beneficiary/victim sets, and legitimacy grounds; per the epsilon-invariance principle they are separate files linked by network edges rather than one story with a measurement parameter. This reading (liberal_nationalist) is the upstream legitimacy-granting node: its self-determination premise is cited BY the religious_zionist and cultural_zionist readings as common ground and AGAINST itself by the settler_colonial and post_zionist readings, so its edges run to all four siblings. Its distinctive structural element — the reciprocal-partition conditionality — is what separates it from the religious reading (which drops conditionality) and the settler-colonial reading (which treats the conditionality as exculpatory cover).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
