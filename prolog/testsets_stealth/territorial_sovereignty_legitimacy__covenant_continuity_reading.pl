% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Covenant-Continuity Legitimacy Doctrine for Territorial Sovereignty
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   A legitimacy doctrine grounds territorial sovereignty in an ancient
 *   covenant (divine promise), continuous Jewish presence, and modern
 *   international recognition (Balfour Declaration 1917, UN Partition Plan
 *   1947, 1948 establishment). This story instantiates ONE reading -
 *   covenant_continuity - of the contested territorial_sovereignty_legitimacy
 *   kernel; the self-determination and existential-matrix readings are
 *   separate constraints with their own epsilon values and victim sets,
 *   linked through the network. The epsilon referent is the standing
 *   sovereignty arrangement as this reading assesses it - never the
 *   arrangement the reading would prefer. Claim/metric independence is
 *   deliberate: claimed_type is mountain because the reading's own ontology
 *   presents the covenant as an unmade, unrevocable grant (a naturality
 *   assertion), while the authored metrics describe contested, actively
 *   defended, substantially extractive operation. Declaring beneficiaries on
 *   the mountain claim is intentional false-summit authoring: the
 *   schema-required omegas document the natural-law-versus-constructed
 *   ambiguity, and the false_summit_mountain signature is expected to
 *   evaluate the gap between the claim and the metric profile.
 *
 * KEY AGENTS:
 *   - - israeli_state_institutions: Agenda-setter (institutional/identity_locked) - administers sovereignty and enforces the legitimacy arrangement it cannot exit
 *   - - religious_zionist_settlement_movement: Primary beneficiary with agenda-setting reach (organized/identity_locked) - converts the doctrine into territorial facts
 *   - - mainstream_israeli_public: Secondary beneficiary (powerful/constrained) - collects belonging and security, bears conscription and censure
 *   - - diaspora_jewish_communities: Mobile beneficiary (organized/mobile) - collects identity and refuge at arbitrage-grade exit cost
 *   - - palestinian_citizens_of_israel: Payer (moderate/constrained) - bears unequal allocation and loyalty scrutiny inside the state
 *   - - west_bank_palestinian_communities: Primary target (powerless/trapped) - bears occupation, land loss, and displacement pressure directly
 *   - - palestinian_refugee_descendants: Intergenerational target (powerless/trapped) - holds a return claim the frame renders inadmissible
 *   - - palestinian_national_institutions: Excluded seat (moderate/trapped) - articulates the rival title from outside the frame
 *   - - international_legal_bodies: Analytical observer (institutional/analytical) - adjudicates under treaty law without bearing costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.7).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, mountain).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Covenant-Continuity Legitimacy Doctrine for Territorial Sovereignty").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).
domain_priors:emerges_naturally(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '83a7dba5-a4e3-4c6c-a657-61e15d955d09').
narrative_ontology:cs_kernel_codification('83a7dba5-a4e3-4c6c-a657-61e15d955d09', fixed_text).
narrative_ontology:cs_authority_grounding('83a7dba5-a4e3-4c6c-a657-61e15d955d09', lineage).
narrative_ontology:cs_interpretation_layer_present('83a7dba5-a4e3-4c6c-a657-61e15d955d09').
narrative_ontology:cs_reading_relation('83a7dba5-a4e3-4c6c-a657-61e15d955d09', territorial_sovereignty_legitimacy__self_determination_reading, influences).
narrative_ontology:cs_reading_relation('83a7dba5-a4e3-4c6c-a657-61e15d955d09', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('83a7dba5-a4e3-4c6c-a657-61e15d955d09', foundational, divine_covenant_confers_irrevocable_territial_title).
narrative_ontology:cs_axiom_status(divine_covenant_confers_irrevocable_territial_title, holdable).
narrative_ontology:cs_axiom_grounding('83a7dba5-a4e3-4c6c-a657-61e15d955d09', divine_covenant_confers_irrevocable_territial_title, theological).
narrative_ontology:cs_axiom('83a7dba5-a4e3-4c6c-a657-61e15d955d09', foundational, international_recognition_confirms_preexisting_right).
narrative_ontology:cs_axiom_status(international_recognition_confirms_preexisting_right, holdable).
narrative_ontology:cs_axiom_grounding('83a7dba5-a4e3-4c6c-a657-61e15d955d09', international_recognition_confirms_preexisting_right, conventional).
narrative_ontology:cs_reference_frame('83a7dba5-a4e3-4c6c-a657-61e15d955d09', irrevocable_covenant_title).
narrative_ontology:cs_drift_state('83a7dba5-a4e3-4c6c-a657-61e15d955d09', contemporary_post_oslo_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('83a7dba5-a4e3-4c6c-a657-61e15d955d09', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, mainstream_israeli_public).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, west_bank_palestinian_communities).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, divine_covenant_title_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, pre_existing_right_confirmation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the state whose founding legitimacy this doctrine supplies: runs the courts, the school curriculum, the settlement administration, and the diplomatic corps that defend the territorial claim abroad. Successive governments adjust the doctrine's application (annexation steps, building freezes, proposed exchanges) but not its premise; repudiating the covenant foundation would dissolve the state's own self-understanding, so the seat cannot leave the arrangement it administers.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Builds and maintains communities across the contested territories, treating residence there as fulfillment of promise rather than as a policy choice. Supplies cadres, ideology, and decisive electoral weight to governing coalitions, and interprets withdrawals as tests of faith. Leaving the project would mean abandoning vows, dismantled communities, and a lifetime's meaning structure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, agenda_setter).

% Draws security, belonging, and historical continuity from the state the doctrine grounds, and votes for or against particular applications of it. Bears conscription, war risk, and international censure as recurring costs of the arrangement. Emigration exists but severs citizenship, family proximity, and social world.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, mainstream_israeli_public, beneficiary,
    powerful, biographical, constrained, national).

% Inherit the covenant narrative as an identity resource and the state as refuge and source of pride; fund institutions and lobby governments on its behalf. Distance from the arrangement is available at far lower cost than for residents: affiliation can be dialed up or down without leaving home.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, biographical, mobile, global).

% Hold formal citizenship inside the state whose founding title narrative places them outside the covenant; navigate loyalty scrutiny, unequal resource allocation, and planning discrimination. Leaving means leaving home; voice operates through courts and elections that the doctrine's premise only partially reaches.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Live under military administration amid expanding communities built under the return-framing; face movement barriers, loss of land access, and demolition orders. There is no pathway to citizenship in the state that governs them and no functioning state of their own; departure would mean permanent dispossession.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, west_bank_palestinian_communities, payer,
    powerless, generational, trapped, regional).

% Descendants of those displaced in 1948, concentrated in camps and exile communities across the region; hold a return claim that the doctrine's title narrative renders inadmissible. Several host states deny them citizenship; the camps persist across generations with no exit that preserves the claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants, payer,
    powerless, generational, trapped, continental).

% Articulate a rival title based on continuous residence and demographic majority during the modern period; negotiate from outside the covenant frame, whose premises give their argument no standing within it. Their participation is confined to objection, which the frame registers as rejection rather than testimony.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_national_institutions, excluded,
    moderate, generational, trapped, regional).

% Assess the territorial claim under treaty law and UN resolutions; issue advisory opinions and resolutions that treat the return-framed communities as unlawful settlements. Their verdicts bind diplomatically rather than doctrinally; they adjudicate without bearing the arrangement's costs.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish national collective around a shared transgenerational title narrative: it sustains national identity and mobilization across dispersion, persecution, and demographic lapse by grounding the political claim in sacred history and confirmed recognition rather than in shifting majorities or great-power favor.
% TRANSFER_FUNCTION: Moves sovereign control, territorial access, and narrative authority from the Arab population of the territory and their descendants to the Jewish national collective; moves the compliance costs of the legitimacy debate onto those whose claims the frame discounts.
% ABSENT_VOICES: Palestinian national institutions and refugee descendants are structurally absent from the covenant frame's internal conversation: their premises (continuous modern residence, demographic majority) are inadmissible within it, so unanimity inside the frame arises partly because the dissenting seats were never admissible in the room.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the state would have to rest its claim solely on juridical and self-determination grounds; the settlement enterprise would lose its constitutive warrant; the religious-national coalition would fracture; and the legitimacy question would reopen immediately in every international forum.
% FOUNDING_PROBLEM: Sustaining a viable national claim to the territory through two millennia of dispersion, during which demographic continuity lapsed and title rested on memory and promise rather than possession; subsequently, converting that claim into recognized sovereignty against a rival residence-based claim.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: international legal bodies corroborate the historical facts of the recognition instruments while disputing their doctrinal interpretation; Palestinian historiography corroborates that the dispersion and demographic-lapse problem was real while denying it licenses the current arrangement; academic historians outside both camps corroborate the factual genealogy. No outside source attests that the founding problem remains unsolved in its original form; adherents attest liveness citing existential threat, critics attest resolution citing achieved sovereignty.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(territorial_sovereignty_legitimacy__covenant_continuity_reading),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the doctrine's operation channels sovereign control and its fruits to the in-group while discounting the out-group's equivalent claim; it is not maximal because the reading's own partition-as-compromise strand concedes the rival claim has weight. Suppression (0.70) is a raw structural property, unscaled by power or scope: it combines state enforcement (military administration, curriculum, loyalty law) with identity fusion among adherents. Theater (0.38) reflects real functional load-bearing (mobilization, policy legitimation) mixed with growing performative maintenance (ceremonial archaeology, liturgical politics, anniversary instrumentation). Accessibility collapse is moderate (0.58): within the committed framework, accepting the covenant premise collapses alternatives almost completely, but the premise itself remains rejectable from outside. Resistance is high (0.68): sustained Palestinian resistance, international legal contestation, and internal post-Zionist critique. The temporal series run on one shared seven-point grid (1917 Balfour, 1947 Partition, 1967 war, 1980 consolidation, 1993 Oslo, 2005 disengagement, present). The trajectory is cyclical rather than monotonic: the Oslo dip (t=76) and the disengagement crisis (t=88) mark crisis-reform-relaxation-accumulation cycles in which each relaxation phase ends with renewed accumulation; the oscillation is not itself the extraction mechanism, but each cycle's endpoint sits higher than the last. Base properties are measured at the interval end, the post-accumulation phase of the latest cycle.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute very differently from the payer seats. From the state seat the doctrine is constitutive order - the thing that makes the polity intelligible to itself - so it experiences near-zero extraction and high legitimacy. From the settlement movement's seat the doctrine is a lived vocation, the deepest subsidy end. From the trapped payer seats the same structure operates as enforced dispossession of claim: their counter-title has no standing inside the frame that governs them. The diaspora seat, with mobile exit, should compute closest to pure beneficiary; the citizen-payer seat, with constrained exit and partial voice, sits between. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: the state (administers and collects legitimation), the settlement movement (collects territorial fulfillment), the public (collects belonging), the diaspora (collects identity and refuge, with arbitrage-grade exit damping effective extraction further). Declared victims map to high directionality: the occupied communities and refugee descendants are trapped and powerless, sitting nearest the full-target end, with regional-to-continental scope amplifying verification difficulty and thus effective extraction. The citizen-payer seat is partially buffered by formal citizenship, placing it below the trapped seats but well above symmetry. International legal bodies occupy the analytical seat and contribute no directional pull. No overrides were needed: the beneficiary/victim declarations plus exit options produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - keeping a national claim alive through dispersion and demographic lapse - was substantially solved by 1948 sovereignty; the doctrine's current center of gravity is foreclosing the rival claim and licensing settlement expansion. Because the status is contested rather than cleanly dead, the mismatch consumer reads contested-status x world_rearranges rather than a clean zombie flag; the honest finding is partial mandatrophy: the preservation function persists as a minority strand (invoked against delegitimization and existential threat) while the foreclosure function dominates practice. The classification prevents mislabeling in both directions: reading the doctrine as pure extraction erases the genuine identity-coordination work that sustains a real community; reading it as pure coordination erases the measurable, enforced asymmetry in whose claims survive contact with it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_grant,
    'Is the covenant title a genuine transcendent grant that would bind regardless of who benefits, or a constructed political doctrine whose force depends on identifiable beneficiaries?',
    'Comparative doctrinal analysis across traditions asserting rival titles to the same territory, testing whether the claim''s persuasive force survives removal of beneficiary interest; historical study of how the doctrine''s application tracks beneficiary advantage across the interval.',
    'If constructed, the false-summit signature stands and the arrangement classifies as hybrid coordination/extraction; if genuinely transcendent within the community of adherence, the classification shifts toward fixed law for that community even as it remains contested outside it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_grant, conceptual, 'Whether the legitimacy foundation is natural/divine law or constructed doctrine benefiting identifiable agents.').

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates one reading (covenant_continuity) of the territorial_sovereignty_legitimacy kernel; what would the sibling readings (self_determination, existential_matrix) change structurally if instantiated?',
    'Generate the sibling stories and compare computed classifications: the self-determination reading relocates the victim set toward the Jewish-collective side and anchors legitimacy in modern residence; the existential-matrix reading drops the juridical instruments entirely and prices extraction as zero-sum survival cost.',
    'Epsilon, victim sets, and per-seat classifications are reading-indexed; comparing siblings measures the kernel''s contest rather than any single reading''s truth, and prevents one reading''s metrics from being mistaken for the topic''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-indexed classification within a contested kernel.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is state-structural (law, military administration, curriculum) versus internalized (identity fusion making exit unthinkable for adherents)?',
    'Post-exit trajectories: track dissenters who renounce the covenant frame (refuser movements, post-religious cohorts) for persistence of loyalty demands and self-censorship after leaving the enforcement environment.',
    'If the internalized share is high, effective suppression exceeds the structural measure and persists beyond enforcement reach; if low, removing state enforcement would rapidly liberalize the frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between structural enforcement and internalized identity fusion.').

omega_variable(
    partition_compromise_status,
    'Within the reading''s own logic, was the acceptance of partition a final compromise of the pre-existing right or a tactical concession that continued settlement may revise?',
    'Doctrinal analysis of authoritative rulings before and after the Oslo accords and the 2005 withdrawal; measure whether sanctioned settlement expansion tracks the tactical-concession interpretation.',
    'If tactical, the measured extraction is doctrinally licensed and the rising trend is stable; if final, expansion deviates from the doctrine''s own terms and the extraction trend reflects capture of the doctrine by its hardest wing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_compromise_status, conceptual, 'Internal-doctrine status of the partition compromise.').

omega_variable(
    palestinian_coalition_potential,
    'Can the fragmented payer seats (citizens, occupied communities, refugee descendants) convert dispersed position into coalition-level resistance sufficient to alter the arrangement?',
    'Historical analysis of unified-front episodes (intifadas, joint lists, boycott campaigns) and their measurable effect on policy compared with fragmentation periods.',
    'Effective coalition power would raise the resistance the arrangement must overcome and lift the payer seats'' computed leverage; persistent fragmentation keeps them near the full-target end despite numbers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_coalition_potential, empirical, 'Coalition conversion potential for fragmented payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0, 107).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_ccr_tr_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(tsl_ccr_tr_t0, observed).
narrative_ontology:measurement(tsl_ccr_tr_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(tsl_ccr_tr_t30, observed).
narrative_ontology:measurement(tsl_ccr_tr_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(tsl_ccr_tr_t50, observed).
narrative_ontology:measurement(tsl_ccr_tr_t63, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 63, 0.32).
narrative_ontology:measurement_basis(tsl_ccr_tr_t63, observed).
narrative_ontology:measurement(tsl_ccr_tr_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 76, 0.33).
narrative_ontology:measurement_basis(tsl_ccr_tr_t76, observed).
narrative_ontology:measurement(tsl_ccr_tr_t88, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 88, 0.35).
narrative_ontology:measurement_basis(tsl_ccr_tr_t88, observed).
narrative_ontology:measurement(tsl_ccr_tr_t107, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 107, 0.38).
narrative_ontology:measurement_basis(tsl_ccr_tr_t107, observed).

% Extraction over time
narrative_ontology:measurement(tsl_ccr_be_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(tsl_ccr_be_t0, observed).
narrative_ontology:measurement(tsl_ccr_be_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(tsl_ccr_be_t30, observed).
narrative_ontology:measurement(tsl_ccr_be_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(tsl_ccr_be_t50, observed).
narrative_ontology:measurement(tsl_ccr_be_t63, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 63, 0.63).
narrative_ontology:measurement_basis(tsl_ccr_be_t63, observed).
narrative_ontology:measurement(tsl_ccr_be_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 76, 0.61).
narrative_ontology:measurement_basis(tsl_ccr_be_t76, observed).
narrative_ontology:measurement(tsl_ccr_be_t88, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 88, 0.64).
narrative_ontology:measurement_basis(tsl_ccr_be_t88, observed).
narrative_ontology:measurement(tsl_ccr_be_t107, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 107, 0.68).
narrative_ontology:measurement_basis(tsl_ccr_be_t107, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsl_ccr_su_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(tsl_ccr_su_t0, observed).
narrative_ontology:measurement(tsl_ccr_su_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(tsl_ccr_su_t30, observed).
narrative_ontology:measurement(tsl_ccr_su_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(tsl_ccr_su_t50, observed).
narrative_ontology:measurement(tsl_ccr_su_t63, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 63, 0.6).
narrative_ontology:measurement_basis(tsl_ccr_su_t63, observed).
narrative_ontology:measurement(tsl_ccr_su_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 76, 0.57).
narrative_ontology:measurement_basis(tsl_ccr_su_t76, observed).
narrative_ontology:measurement(tsl_ccr_su_t88, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 88, 0.61).
narrative_ontology:measurement_basis(tsl_ccr_su_t88, observed).
narrative_ontology:measurement(tsl_ccr_su_t107, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 107, 0.7).
narrative_ontology:measurement_basis(tsl_ccr_su_t107, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'legitimacy of Israeli territorial sovereignty'. The label conflates three structurally distinct claims: covenant-continuity (this story; temporally anchored title surviving demographic absence, recognition as confirmation), self-determination (modern residence and majority as title source; recognition as creation), and existential-matrix (non-juridical survival logic; instruments irrelevant). Their epsilon values differ widely because the victim sets differ: this reading discounts the Arab-population claim, the self-determination reading discounts the Jewish-historical claim, the existential reading prices both as survival costs. This story is the historically upstream member (its claim predates and is cited by the others' debates); its settlement-as-return practice creates downstream pressure on the self-determination reading's feasibility conditions. Each member links the others via affects_constraints; no member's metrics should be averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
