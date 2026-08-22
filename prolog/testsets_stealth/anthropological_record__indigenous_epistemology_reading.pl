% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Community Authority over the Anthropological Record (Indigenous Epistemology Reading)
 *   domain: epistemology/anthropology/law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the institutional
 *   custody-and-study regime over ancestral remains, burial places, and the
 *   deep human past: museums and universities hold large legacy collections
 *   assembled under nineteenth-century salvage premises, credentialed
 *   disciplines set the terms of interpretation, and — since the 1990
 *   repatriation statute — communities hold statutory standing to demand
 *   return and consultation. The indigenous epistemology reading of the
 *   anthropological record asserts that the record discloses living
 *   relational continuity between present communities, their ancestors, and
 *   their places, knowable through sustained oral tradition. Assessed by that
 *   reading's own lights, the custody regime is a hybrid: it performs real
 *   coordination (inventories, consultation channels, mandated returns) while
 *   continuing to extract custody, interpretive authority, and knowledge
 *   value from the communities whose dead and places it administers. The
 *   claimed type and the metrics are independent authored facts: tangled_rope
 *   is asserted from the structure (genuine coordination function plus
 *   asymmetric extraction plus active enforcement); the metric values
 *   describe the arrangement's operation as this reading assesses it. The
 *   engine computes per-seat classifications from the structural data below.
 *   KEY AGENTS (by structural relationship): - research_museums: Primary
 *   beneficiary and custodian-agenda-setter (institutional/arbitrage) — holds
 *   the collections, sets access terms, collects prestige and funding -
 *   academic_archaeologists: Secondary beneficiary bearing compliance costs
 *   (organized/identity_locked) — careers built on credentialed reading of
 *   the past - heritage_management_agencies: Agenda-setter
 *   (institutional/constrained) — administers the statutory consultation and
 *   repatriation machinery - indigenous_communities: Primary target
 *   (organized/identity_locked) — bears loss of custody, authority, and
 *   ancestors - traditional_knowledge_holders: Target of knowledge extraction
 *   (moderate/identity_locked) — testimony subordinated or translated -
 *   unrecognized_tribes: Excluded seat (powerless/trapped) — ancestral
 *   connection without standing in the process - heritage_ethics_reviewers:
 *   Analytical observer (analytical/analytical) — examines allocation of
 *   custody and knowledge authority
 *
 * KEY AGENTS:
 *   - research_museums: primary beneficiary and custodian-agenda-setter (institutional/arbitrage)
 *   - academic_archaeologists: secondary beneficiary bearing compliance costs (organized/identity_locked)
 *   - heritage_management_agencies: agenda-setter administering the statutory regime (institutional/constrained)
 *   - indigenous_communities: primary target (organized/identity_locked)
 *   - traditional_knowledge_holders: target of knowledge extraction (moderate/identity_locked)
 *   - unrecognized_tribes: excluded seat with ancestral connection but no standing (powerless/trapped)
 *   - heritage_ethics_reviewers: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.72).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.55).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Community Authority over the Anthropological Record (Indigenous Epistemology Reading)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/anthropology/law").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '3f481905-c290-4238-a546-2c022cdbbd0a').
narrative_ontology:cs_kernel_codification('3f481905-c290-4238-a546-2c022cdbbd0a', distributed).
narrative_ontology:cs_authority_grounding('3f481905-c290-4238-a546-2c022cdbbd0a', lineage).
narrative_ontology:cs_interpretation_layer_present('3f481905-c290-4238-a546-2c022cdbbd0a').
narrative_ontology:cs_reading_relation('3f481905-c290-4238-a546-2c022cdbbd0a', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('3f481905-c290-4238-a546-2c022cdbbd0a', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('3f481905-c290-4238-a546-2c022cdbbd0a', foundational, oral_tradition_epistemically_necessary).
narrative_ontology:cs_axiom_status(oral_tradition_epistemically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3f481905-c290-4238-a546-2c022cdbbd0a', oral_tradition_epistemically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('3f481905-c290-4238-a546-2c022cdbbd0a', foundational, community_authority_over_ancestral_remains).
narrative_ontology:cs_axiom_status(community_authority_over_ancestral_remains, holdable).
narrative_ontology:cs_axiom_grounding('3f481905-c290-4238-a546-2c022cdbbd0a', community_authority_over_ancestral_remains, deontological).
narrative_ontology:cs_reference_frame('3f481905-c290-4238-a546-2c022cdbbd0a', relational_continuity_with_ancestors_and_place).
narrative_ontology:cs_drift_state('3f481905-c290-4238-a546-2c022cdbbd0a', contemporary_post_repatriation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3f481905-c290-4238-a546-2c022cdbbd0a', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, research_museums).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, academic_archaeologists).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, heritage_management_agencies).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, traditional_knowledge_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, academic_archaeologists).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, scientific_method_supremacy_over_the_past).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, verifiability_gatekeeping_of_historical_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold large legacy collections of ancestors and cultural items acquired under nineteenth- and early-twentieth-century premises; set access policy, loan terms, and the pace and scope of returns; publish, exhibit, and fundraise on the holdings. They can comply minimally, restructure storage, transfer custody while retaining curatorial roles, or relocate holdings across jurisdictions — exit costs are real but they hold the assets everyone else is negotiating over.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, research_museums, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, research_museums, beneficiary).

% Build careers on excavation, skeletal series, and site interpretation, drawing salary, publication, and standing from access to the record. Since the repatriation statute they must obtain permits, consult, and accept closure of some research lines (unaffiliated remains, certain ancient-DNA work). Leaving the discipline means abandoning trained expertise; professional identity is bound to being the credentialed reader of the past, so the costs they bear are borne inside a commitment they cannot easily set down.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, academic_archaeologists, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, academic_archaeologists, payer).

% Administer the statutory regime: inventories, consultation deadlines, grant conditions, review committees, and enforcement referrals. They gain administrative function and budget from operating the process and are constrained by statute, appropriations, and political direction; they neither hold the collections nor bear the losses, but they set much of the procedural terrain on which every other seat acts.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, heritage_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the arrangement's costs: ancestors and items held in distant institutions, burial places disturbed or flooded, claims requiring documentary proof assembled in credentialed forms. Since 1990 they hold statutory standing to request return and consultation and have organized through tribal governments and intertribal bodies to use it. They cannot exit the relationship to their own dead and places without abandoning who they are; the option set is participation on the process's terms or refusal at cost.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_communities, payer,
    organized, generational, identity_locked, continental).

% Carry the oral traditions that locate peoples in place and time across generations. Their testimony was historically dismissed as myth; where it is now admitted, it must be translated into evidentiary formats, and material recorded in consultations can circulate in reports, databases, and archives beyond community control. Their authority comes from transmission they cannot delegate, so participation exposes the knowledge itself while refusal silences the community's voice in determinations about its own dead.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, traditional_knowledge_holders, payer,
    moderate, generational, identity_locked, regional).

% Communities without federal recognition hold ancestral connections to remains and places governed by the process but lack standing in it: consultation duties run to recognized tribes, and their claims rarely enter the record. They bear the arrangement's costs — holding of ancestors, exclusion from decisions — with no procedural channel through which to object; their absence is a condition the process's rules produce, not a choice they made.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, unrecognized_tribes, excluded,
    powerless, generational, trapped, regional).

% Philosophers of science, museologists, and ethicists who examine how custody, consent, and knowledge authority are allocated in the governance of the dead. They take testimony from every seat, publish analyses none of the parties controls, and hold no custody, revenue, or standing in the determinations they study.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, heritage_ethics_reviewers, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, research_museums).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates custody, documentation, study, and disposition of ancestral remains and cultural items across hundreds of institutions: centralized inventories, provenance standards, consultation channels, and statutory repatriation procedures determine who may hold, examine, and decide — problems that would otherwise be litigated case-by-case without shared process.
% TRANSFER_FUNCTION: Moves physical custody of ancestors and items, interpretive authority over the deep past, and the publication and prestige value derived from sites and oral traditions, from Indigenous communities to museums, academic disciplines, and agencies; since 1990 a mandated counter-flow returns a portion of custody and compels consultation in the other direction.
% ABSENT_VOICES: Non-federally-recognized tribes hold ancestral connections but little standing in the statutory process; their objections surface only intermittently, through advocacy intermediaries. Elders of earlier generations, whose testimony was never recorded before the collections were assembled, are permanently absent; their objections survive only as community memory carried by the knowledge holders who remain.
% DISAPPEARANCE_RATIONALE: If the custody-and-consultation regime vanished overnight, institutions would revert to unilateral control of holdings, research access would reopen without consent requirements, pending claims would collapse into ordinary property litigation where institutions hold title, and communities would lose the statutory lever that has returned tens of thousands of ancestors — the entire governance field would reorganize around raw institutional possession.
% FOUNDING_PROBLEM: Salvage anthropology: the nineteenth-century premise that Indigenous peoples were vanishing, so their remains and items had to be collected and preserved by institutions before the peoples — presumed to leave no surviving descendants with rights — disappeared.
% FOUNDING_PROBLEM_CORROBORATION: Congressional findings in the 1990 statute's legislative history expressly reject the extinction premise and the denial of descendant rights; tribal testimony to Congress and successive museum-sector ethical reviews — sources outside the benefiting parties' control — concur that the founding premises failed. No credible source outside the benefiting parties attests that the original salvage problem remains live; the justifications offered for continued institutional custody are successor arguments, not the founding one.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the arrangement's core asymmetry persists: hundreds of thousands of ancestors remain institutionally held, the burden of proving cultural affiliation falls on the communities rather than on the institutions claiming the right to hold, and admissibility of oral tradition in determinations remains contested and partial. Suppression (0.55) is structural rather than theatrical: legal barriers, funding asymmetries, and credential gatekeeping raise the cost of community claims, though statutory counter-channels now exist. Theater ratio (0.32) reflects a compliance-era layer — consultation conducted to satisfy procedural checkboxes — sitting on top of a real repatriation function. Accessibility collapse is moderate-low (0.4): alternatives persist (litigation, tribally controlled museums, negotiated research partnerships, international norms), so understanding the regime does not close every exit. Resistance is high (0.7): the arrangement faces decades of organized challenge — the repatriation movement, intertribal coalitions, legislative campaigns, and refusals of research access — and has already been forced to concede statutory ground. The temporal series share one grid (t = 0, 20, 45, 70, 95, 105, 115, 125, 135 on a 1890-to-2025 mapping): extractiveness declines slowly throughout and steps down after the 1990 statute; theater peaks in the compliance-building decade after 1990 and recedes as substantive transfers scale; suppression_requirement is authored because enforcement capacity is the traced dynamic — it rises through the mid-century consolidation of institutional control (state-backed collecting, termination-era site destruction) and falls after 1990 as statutory counter-enforcement arms the communities.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute differently. From the museum and agency positions the arrangement is stewardship: fragile material held against loss, studied under professional standards, returned where law requires. From the community positions the same structure is continued dispossession administered with paperwork. A sharper same-level divergence sits between academic_archaeologists and indigenous_communities, both carrying the organized power atom: their situations differ not by global standing but by which forum's evidentiary rules count and by the direction of their identity locks — the discipline's members are fused to being the credentialed readers of the past, the communities to being the relatives of the dead, so neither can treat the other's frame as a mere negotiable position. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Research_museums sit nearest the beneficiary end: they receive custody, prestige, and funding, and their arbitrage-grade exit (restructuring, transferring, minimal compliance) damps effective extraction further. Heritage_management_agencies are agenda-setters with modest administrative gain and low d. Academic_archaeologists are beneficiaries who also bear real costs (closed research lines, compliance burdens), placing them beneficiary-side but not at the floor. Indigenous_communities carry high directionality toward full target: they bear the transfer of custody and authority, and their identity_locked exit amplifies effective extraction — they cannot walk away from their own dead and places. Traditional_knowledge_holders are similarly targeted, with the added mechanism that recorded testimony circulates beyond their control. Unrecognized_tribes, excluded and trapped, bear costs with no channel, putting them at the extreme target end despite their formal absence from the process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — salvage collection before the presumed extinction of peoples presumed to leave no rights-holding descendants — is dead: its factual premise failed, and the statutory record itself says so. Yet the arrangement persists under successor justifications (stewardship, comparative science, administrative necessity). That dead-problem-plus-persistence profile raises the capture/zombie flag, cross-checked here against the computed path: theater_ratio of 0.32 and a live repatriation function argue against a piton reading, and the concentrated receipt of gains in the museum seat argues against benign neglect. The classification guards against two opposite errors: reading the whole arrangement as snare ignores the genuine coordination achieved (shared inventories, consultation channels, tens of thousands of returns that would not otherwise have occurred), while reading it as rope ignores the persistent asymmetry of burden and custody. Tangled_rope holds both truths: the same structure that returns ancestors also keeps the majority of them, and the same consultation channel that admits testimony also translates and extracts it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the indigenous_epistemology_reading of the kernel anthropological_record; what would the sibling readings (naturalist_reading, creationist_reading) change structurally if they governed instead?',
    'Compare the three compiled stories: the naturalist reading shifts the victim set toward whoever blocks material access and relocates authority to credentialed method; the creationist reading shifts the evidentiary kernel to scriptural compatibility. The disagreement is located in what counts as evidence and who counts as a knower.',
    'If a sibling reading governed, the beneficiary/victim structure inverts or dissolves: under the naturalist reading the communities'' statutory leverage becomes the obstruction, and under the creationist reading both credentialed and community authority are subordinated to scriptural adjudication. Per-seat classifications computed here hold only for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    affiliation_burden_placement,
    'Is the placement of the cultural-affiliation burden on communities an intrinsic feature of the arrangement, or a removable rule choice that a statute or regulation could reverse?',
    'Statutory amendment history and pilot regimes that invert the burden (institutions must justify continued holding); measure claim success rates and institutional compliance under each design.',
    'If the burden is removable by rule, a large share of the measured extraction is policy-contingent rather than structural, and the arrangement''s classification could move toward the coordination-dominant end; if intrinsic, the extraction is load-bearing for the beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affiliation_burden_placement, preference, 'Whether the extraction-driving burden of proof is contingent rule design or structural core.').

omega_variable(
    oral_tradition_concordance_scope,
    'How far does the documented concordance between sustained oral traditions and independent physical evidence (volcanic events preserved in narrative, coastal submergence accounts, migration-sequence testimonies) generalize across domains and communities?',
    'Systematic comparison of transmitted testimonies against geological, paleoenvironmental, and archaeological records under community-controlled protocols, with negative results reported at the same rate as positive.',
    'Wide generalization strengthens the reading''s epistemic-necessity axiom and raises the cost of excluding testimony; narrow scope would confine the axiom to specific classes of events and weaken the claim that material evidence is insufficient without oral tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_concordance_scope, empirical, 'Empirical reach of the reliability premise underlying the reading''s foundational axiom.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of community knowledge claims structural (legal standing limits, funding asymmetries, credential gatekeeping) or partly internalized (communities pre-translating claims into credentialed evidentiary forms because the hierarchy has been absorbed)?',
    'Post-statute trajectory analysis: as formal barriers fall, observe whether communities continue to volunteer Western-format documentation unprompted, and whether tribally controlled review bodies relax translation practices over time.',
    'If internalized, effective suppression exceeds the structural measure and persists after barrier removal — the constraint travels with the targets; if purely structural, removal of the barriers collapses the suppression term quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized component of the suppression holding community claims down.').

omega_variable(
    consultation_as_extraction_channel,
    'Does the consultation apparatus itself function as a knowledge-extraction channel — recording ceremony details, site locations, and testimony into agency files and reports that circulate beyond community control?',
    'Audit of consultation-generated records: where they are stored, who accesses them, whether community-imposed data-sovereignty agreements bind downstream use.',
    'If consultation substitutes one extraction form for another, the post-1990 decline in measured extractiveness overstates the improvement, and the arrangement''s coordination function is partly a delivery mechanism for the extraction it replaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consultation_as_extraction_channel, empirical, 'Whether the remedy channel reproduces the extraction it was built to end.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 135).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__indigenous_epistemology_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(anth_tr_t45, anthropological_record__indigenous_epistemology_reading, theater_ratio, 45, 0.19).
narrative_ontology:measurement(anth_tr_t70, anthropological_record__indigenous_epistemology_reading, theater_ratio, 70, 0.26).
narrative_ontology:measurement(anth_tr_t95, anthropological_record__indigenous_epistemology_reading, theater_ratio, 95, 0.33).
narrative_ontology:measurement(anth_tr_t105, anthropological_record__indigenous_epistemology_reading, theater_ratio, 105, 0.41).
narrative_ontology:measurement(anth_tr_t115, anthropological_record__indigenous_epistemology_reading, theater_ratio, 115, 0.38).
narrative_ontology:measurement(anth_tr_t125, anthropological_record__indigenous_epistemology_reading, theater_ratio, 125, 0.35).
narrative_ontology:measurement(anth_tr_t135, anthropological_record__indigenous_epistemology_reading, theater_ratio, 135, 0.32).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.86).
narrative_ontology:measurement(anth_be_t20, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(anth_be_t45, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 45, 0.83).
narrative_ontology:measurement(anth_be_t70, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 70, 0.81).
narrative_ontology:measurement(anth_be_t95, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 95, 0.79).
narrative_ontology:measurement(anth_be_t105, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 105, 0.76).
narrative_ontology:measurement(anth_be_t115, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 115, 0.74).
narrative_ontology:measurement(anth_be_t125, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 125, 0.73).
narrative_ontology:measurement(anth_be_t135, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 135, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anth_su_t20, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(anth_su_t45, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(anth_su_t70, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 70, 0.74).
narrative_ontology:measurement(anth_su_t95, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 95, 0.71).
narrative_ontology:measurement(anth_su_t105, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 105, 0.63).
narrative_ontology:measurement(anth_su_t115, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 115, 0.59).
narrative_ontology:measurement(anth_su_t125, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 125, 0.57).
narrative_ontology:measurement(anth_su_t135, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 135, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, resource_allocation).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the anthropological record' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story constraint family: the naturalist reading (materialist origins via scientific method), the creationist reading (divine creation compatible with scripture), and this indigenous epistemology reading (relational continuity knowable via sustained oral tradition). Each story carries its own epsilon, its own beneficiary/victim structure, and its own knowledge channel; measuring the record through one reading's observable yields a different epsilon than another's precisely because they are different constraints. This reading authors epsilon for the standing institutional-custody arrangement as it assesses that arrangement; the sibling stories author epsilon for their own contested arrangements by their own lights. Upstream/downstream coupling runs through repatriation outcomes: this reading's statutory victories remove materials from the naturalist reading's evidentiary base, which is why the influence edge points from this story to the naturalist sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
