% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of the Anthropological Record: Material Human Origins Knowable via Scientific Method
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   A century-and-a-half-old arrangement assigns interpretive authority over
 *   the human-origin record — fossils, strata, genetic variation — to
 *   credentialed specialists working under methodological-naturalist rules,
 *   enforced through journal review, hiring, funding, permits, and custody
 *   policy. The arrangement solves a real problem: without standardized
 *   method and mutual verification, claims about the deep past dissolve into
 *   competing testimonies with no arbiter. It also concentrates careers,
 *   grants, publication standing, and custody of the dead inside the
 *   beneficiary class, while oral-tradition holders, faith-based readers, and
 *   unaffiliated amateurs bear denial costs at the gate. This file
 *   instantiates ONE reading of the anthropological_record kernel — the
 *   naturalist reading — as a clean epsilon-invariant constraint per DP-001:
 *   the sibling readings are other constraints, linked in the network, not
 *   folded into this one. Epsilon's referent is the standing arrangement
 *   under contest (the institutionalized naturalist interpretive monopoly),
 *   and its value is reading-indexed: assessed by the naturalist reading's
 *   own lights, which endorse the core while registering the gatekeeping
 *   asymmetry — hence moderate-high, not the near-total figure a rival
 *   reading would author over the same referent. KEY AGENTS (by structural
 *   relationship): - academic_gatekeeping_institutions: Agenda-setter
 *   (institutional/arbitrage) — administers review, funding, permits; absorbs
 *   defense costs - credentialed_paleoanthropologists: Primary beneficiary
 *   (organized/identity_locked) — careers and standing flow through the
 *   arrangement - research_universities_museums: Secondary beneficiary
 *   (institutional/mobile) — tuition, overhead, custody, prestige -
 *   indigenous_knowledge_holders: Primary target (organized/identity_locked)
 *   — traditions ruled out of bounds, ancestors held in custody -
 *   creationist_faith_communities: Target (organized/identity_locked) —
 *   maintains parallel institutions at own expense -
 *   independent_amateur_researchers: Target (moderate/constrained) — finds
 *   surface, interpretations denied standing - general_public: Diffuse
 *   beneficiary-payer (moderate/constrained) — receives output, funds it, no
 *   standards seat - philosophy_of_science_scholars: Analytical observer —
 *   examines the warrant structure itself
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.6).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.66).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record: Material Human Origins Knowable via Scientific Method").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '7adaa748-2d85-4b5a-bc95-f5b939a56076').
narrative_ontology:cs_kernel_codification('7adaa748-2d85-4b5a-bc95-f5b939a56076', formalized).
narrative_ontology:cs_authority_grounding('7adaa748-2d85-4b5a-bc95-f5b939a56076', expertise).
narrative_ontology:cs_interpretation_layer_present('7adaa748-2d85-4b5a-bc95-f5b939a56076').
narrative_ontology:cs_reading_relation('7adaa748-2d85-4b5a-bc95-f5b939a56076', anthropological_record__creationist_reading, influences).
narrative_ontology:cs_reading_relation('7adaa748-2d85-4b5a-bc95-f5b939a56076', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('7adaa748-2d85-4b5a-bc95-f5b939a56076', foundational, supernatural_causation_outside_scientific_warrant).
narrative_ontology:cs_axiom_status(supernatural_causation_outside_scientific_warrant, holdable).
narrative_ontology:cs_axiom_grounding('7adaa748-2d85-4b5a-bc95-f5b939a56076', supernatural_causation_outside_scientific_warrant, instrumental).
narrative_ontology:cs_axiom('7adaa748-2d85-4b5a-bc95-f5b939a56076', foundational, credentialed_interpretive_authority).
narrative_ontology:cs_axiom_status(credentialed_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7adaa748-2d85-4b5a-bc95-f5b939a56076', credentialed_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('7adaa748-2d85-4b5a-bc95-f5b939a56076', naturalist_deep_time_material_archive).
narrative_ontology:cs_drift_state('7adaa748-2d85-4b5a-bc95-f5b939a56076', contemporary_repatriation_era, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('7adaa748-2d85-4b5a-bc95-f5b939a56076', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_paleoanthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, research_universities_museums).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, general_public).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_faith_communities).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, independent_amateur_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Journal editors, tenure and hiring committees, federal funding panels, and museum curatorial boards decide which accounts of human origins enter the literature, receive grants, and shape exhibits. They administer peer review, set the methodological requirements a submission must meet, and control excavation permits. When challenges arrive from outside the profession — legislative curriculum bills, repatriation demands, rival institutions — they coordinate the response and absorb the administrative cost of defending the arrangement.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, academic_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Specialists whose decade-plus training ran through university programs teaching descent-and-deep-time methods. Salaries, grants, publication records, and professional standing all flow through the arrangement; their interpretive habits, citations, and social networks are bound up with it. Leaving for another line of work would forfeit the accumulated investment; remaining requires keeping pace with the literature the gatekeepers certify.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_paleoanthropologists, beneficiary,
    organized, biographical, identity_locked, global).

% Host departments and collections receiving tuition, grant overhead, donor prestige, and visitor revenue tied to origin narratives. They hold custody of skeletal collections and artifacts assembled during colonial-era expeditions. Under legal and ethical pressure they negotiate loans, repatriations, and co-curation agreements, trading portions of custody for continued operating legitimacy.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, research_universities_museums, beneficiary,
    institutional, generational, mobile, global).

% Nations and lineages whose oral traditions carry accounts of ancestry, migration, and relationship to place. The technical literature has treated those traditions as folklore or raw data to mine rather than as interpretive testimony, and for over a century ancestral remains were excavated, shipped, and displayed without consent. Repatriation law, tribal review boards, and co-governance agreements are opening seats at the table, but editorial, funding, and permitting decisions remain seated elsewhere. Exiting into the mainstream frame would mean surrendering the tradition itself, so participation comes on renegotiated terms or not at all.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, indigenous_knowledge_holders, excluded).

% Congregational networks and institutes that read the same geological and fossil deposits as evidence of designed origins on a compressed timeline. Barred from technical journals and most university posts, they sustain schools, presses, museums, and accredited colleges of their own, and pursue curriculum influence through legislatures and school boards. Their parallel institutions duplicate, at their own expense, functions the mainstream provides its members directly.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_faith_communities, payer,
    organized, generational, identity_locked, national).

% Unaffiliated fossil hunters, site stewards, and local historians who make genuine finds and hold regional knowledge. Without institutional affiliation their manuscripts face desk rejection, specimens they surface pass into museum custody, and fieldwork permits require credentialed sponsorship. Citizen-science programs admit some under supervision; independent interpretive standing remains largely unavailable.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, independent_amateur_researchers, payer,
    moderate, biographical, constrained, regional).

% Taxpayers and audiences who receive the arrangement's output through school curricula, documentaries, and museum exhibits. They fund the enterprise and vote intermittently on curriculum controversies, but hold no seat in setting technical standards. Their children's textbooks are the proximate prize contested in public, and they carry the indirect costs of those contests.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, general_public, beneficiary,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, general_public, payer).

% Analysts of warrant and demarcation who examine what licenses the arrangement's exclusivity claims, how testimony from excluded parties is weighed, and where methodological rules shade into institutional preference. They hold no excavation permits and award no grants; their leverage runs through critique, history of science, and occasional advisory roles.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, philosophy_of_science_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, credentialed_paleoanthropologists).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes training, dating methods, excavation protocol, and peer verification so that claims about human origins accumulate and cross-check across labs and generations instead of dissolving into mutually unintelligible testimonies. The problem solved is distinguishing durable inference about the deep past from noise at civilizational scale.
% TRANSFER_FUNCTION: Moves interpretive authority, publication space, grant funds, excavation permits, and custody of human remains and artifacts away from uncredentialed interpreters — faith communities, oral-tradition holders, independent amateurs — toward credentialed specialists and their host institutions; moves public trust and tax revenue in the same direction.
% ABSENT_VOICES: Indigenous knowledge holders were historically outside editorial boards, funding panels, and curatorial decisions — admitted as subjects and data sources, rarely as adjudicators; they are now entering through repatriation regimes and co-governance demands. Creationist interpreters are loudly present in public discourse but structurally absent from technical venues. Descendant communities of the studied dead lacked any seat until recent legal changes.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, there would be no shared chronology, no verified descent narrative, no certified training pipeline; museums, journals, and careers would lose their organizing spine, rival readings would rush the vacuum, and applied fields that consume evolutionary frameworks (epidemiology, crop science, conservation biology) would lose their evidentiary supply line. The world rearranges around whatever replaces the verification function.
% FOUNDING_PROBLEM: Mid-nineteenth-century crisis over human antiquity: scriptural chronologies collided with accumulating fossil and geological evidence, dating was unreliable, and competing testimonies — clerical, aristocratic-amateur, emerging professional — carried equal social weight with none carrying verified warrant. The arrangement was built to make claims about human origins publicly checkable and cumulative rather than resting on inherited authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science (external to the benefiting parties) document the founding crisis and its resolution in the professionalization record; applied-science communities — epidemiologists, agronomists, conservation biologists — attest the continuing utility of the arrangement's outputs without themselves collecting its rents; several rival-reading parties concede the dating methods' operational reliability even while disputing the interpretive frame. Corroboration exists outside the beneficiary set.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: the asymmetry is substantial — careers, grants, publication space, and custody concentrate in the credentialed class while excluded parties pay in kind (traditions dismissed, manuscripts desk-rejected, specimens transferred, curricula fought over) — but the arrangement simultaneously delivers a verification service every seat consumes, which tempers the reading's own assessment below what a rival seat would author. Suppression 0.66: enforcement is continuous and structural (review filters, hiring norms, funding priorities, permit routing) but non-violent, with parallel-institution exits available at heavy status cost. Theater 0.31: most activity is functional laboratory and field labor; a growing minority is ritual defense — apologetic volumes, textbook-war mobilizations, ceremonial debunking aimed at audiences rather than inquiry. Accessibility_collapse 0.52: alternatives persist (parallel academies, oral traditions, citizen science) but are closed out of the certifying venues, so collapse is venue-relative rather than total. Resistance 0.68: organized movements, litigation, repatriation politics, and parallel institutions constitute sustained, well-resourced pushback. The temporal series run on one shared nine-point grid across all three tracked metrics (alignment rule): extractiveness climbs through professionalization (the Victorian gentleman-naturalist era was comparatively permeable — Darwin himself held no credential), peaks with the late-century enforcement ratchet, and softens slightly as open access, preprints, and co-governance erode the monopoly; suppression_requirement traces the enforcement-infrastructure build-up and partial decay, which is why that series is authored rather than left static; theater oscillates with public-contest cycles (Scopes-era apologetics, curriculum wars) rather than drifting monotonically.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the beneficiary seat, the arrangement is earned authority: a decade of training bought the standing, and the gate is quality control. From the payer seats, the same gate is a wall: their accounts never reach the venues regardless of content, and custody of ancestors was taken without consent. From the agenda-setter seat, it is stewardship — someone must hold the line against both crank intrusion and political capture. The engine computes these divergent per-seat classifications from the structural data (power, exit, role); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionalities: gatekeeping institutions (arbitrage exit, they set the rules) sit nearest the beneficiary pole; credentialed specialists derive low d from beneficiary status, with identity_lock modulating upward modestly — they are net gainers whose exit is expensive, not targets; universities and museums similar with mobile exit. Declared victims map to high d: indigenous holders and faith communities carry identity_locked exits (their readings are constitutive of who they are — exit means abandoning the tradition), which pins them near the full-target end; amateurs, merely constrained, sit slightly lower. The general public derives near-symmetric-low: genuine receipt of coordinated knowledge, diffuse tax cost, no venue power. Scope amplification applies modestly at global scale — verification is harder to audit across continents, so the engine scales effective extraction upward for the far-flung targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making origins-knowledge cumulative and publicly checkable rather than resting on inherited authority — remains live, so no zombie declaration issues: founding_problem_status=live crossed with disappearance_verdict=world_rearranges produces no capture/zombie flag. The classification guards both error directions: calling this a snare would erase the real, consumed-by-all verification service (predictions like Tiktaalik-grade fossil finds were made and confirmed); calling it a rope would erase the documented exclusion costs borne by three identified victim classes. Tangled rope is the honest center: coordination function present, asymmetric extraction riding on it, active enforcement holding both. The piton risk lives at long horizons via paradigm identity fusion (see omega) — if the coordination function were someday carried by lighter infrastructure while the gate persisted theatrically, the structure would migrate toward piton; nothing in the current record shows that state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which structural features authored here belong to the naturalist reading specifically versus the shared kernel, and how would the sibling readings redistribute beneficiaries, victims, and epsilon?',
    'Generate and compile the sibling stories (anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading) and compare per-seat classifications across the family.',
    'If the credentialing-gate asymmetry is reading-specific, the siblings compute different types and different victim sets; if the asymmetry is kernel-wide, the family shares a classification profile and the network contamination analysis dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is one reading of the anthropological_record kernel; committer structure routed here per the committer-frame rules.').

omega_variable(
    evidence_compulsion_vs_constructed_exclusion,
    'Does excluding supernatural causation reflect evidence-compelled closure (rival explanations repeatedly failed when fairly tested) or institutional construction protecting interpretive turf?',
    'History-of-science case analysis of whether rival readings received genuine empirical hearings or constitutional dismissal; audit of episodes where design-style hypotheses were actually tested versus rejected at the door.',
    'If closure is evidence-compelled, the empirical core trends toward mountain-like certification and the extraction concentrates in the gatekeeping layer alone; if constructed, the whole arrangement reads as more heavily extracted and FSM-adjacent dynamics strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_compulsion_vs_constructed_exclusion, conceptual, 'Whether the naturalist exclusivity claim is discovery-forced or institutionally maintained.').

omega_variable(
    gate_verification_separability,
    'Is the credentialing gate separable from the verification function it protects — could non-credentialed interpreters contribute without degrading reliability?',
    'Examine validated counterexamples: amateur discoveries accepted into the literature, oral tradition successfully guiding excavation, community-reviewed publication experiments; measure whether reliability degraded where the gate opened.',
    'If separable, a large share of the arrangement''s cost to excluded parties purchases boundary maintenance rather than reliability, raising effective extraction; if inseparable, part of that cost is the irreducible price of coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gate_verification_separability, conceptual, 'Structural separability of the credential boundary from the verification service.').

omega_variable(
    internalized_self_censorship,
    'Is the low rate of excluded-interpreter participation driven by structural venue denial, or by internalized anticipation of dismissal that persists where channels open?',
    'Post-opening trajectory analysis: track submissions and outcomes after access channels expand (community review boards, co-authored monographs, indigenous-led journals); if participation stays low after structural barriers drop, the deficit is internalized.',
    'If internalized, effective suppression exceeds the structural measure and outlasts barrier removal; a classification treating the arrangement''s suppression as purely structural would understate its reach and misdate any decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_self_censorship, empirical, 'Structural versus internalized suppression among excluded interpreters.').

omega_variable(
    paradigm_identity_fusion,
    'How much of the specialists'' attachment to the materialist frame is professional identity fusion (career path dependence) versus evidential conviction?',
    'Compare interpretive behavior of specialists whose funding and careers decoupled from the frame (emeritus, cross-appointed, industry-moved) against active-career specialists; divergence indicates fusion-weighted attachment.',
    'High fusion means the beneficiary seat''s derived low directionality understates its stake in the arrangement''s persistence — the structure would persist by inertia even under shifting evidential pressure, pulling the classification toward piton dynamics over long horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_identity_fusion, conceptual, 'Identity-lock composition of the primary beneficiary seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__naturalist_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(anth_tr_t60, anthropological_record__naturalist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(anth_tr_t80, anthropological_record__naturalist_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(anth_tr_t100, anthropological_record__naturalist_reading, theater_ratio, 100, 0.26).
narrative_ontology:measurement(anth_tr_t120, anthropological_record__naturalist_reading, theater_ratio, 120, 0.32).
narrative_ontology:measurement(anth_tr_t140, anthropological_record__naturalist_reading, theater_ratio, 140, 0.3).
narrative_ontology:measurement(anth_tr_t160, anthropological_record__naturalist_reading, theater_ratio, 160, 0.31).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(anth_be_t20, anthropological_record__naturalist_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(anth_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(anth_be_t60, anthropological_record__naturalist_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(anth_be_t80, anthropological_record__naturalist_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(anth_be_t100, anthropological_record__naturalist_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement(anth_be_t120, anthropological_record__naturalist_reading, base_extractiveness, 120, 0.65).
narrative_ontology:measurement(anth_be_t140, anthropological_record__naturalist_reading, base_extractiveness, 140, 0.63).
narrative_ontology:measurement(anth_be_t160, anthropological_record__naturalist_reading, base_extractiveness, 160, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(anth_su_t20, anthropological_record__naturalist_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(anth_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(anth_su_t60, anthropological_record__naturalist_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(anth_su_t80, anthropological_record__naturalist_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(anth_su_t100, anthropological_record__naturalist_reading, suppression_requirement, 100, 0.74).
narrative_ontology:measurement(anth_su_t120, anthropological_record__naturalist_reading, suppression_requirement, 120, 0.72).
narrative_ontology:measurement(anth_su_t140, anthropological_record__naturalist_reading, suppression_requirement, 140, 0.68).
narrative_ontology:measurement(anth_su_t160, anthropological_record__naturalist_reading, suppression_requirement, 160, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the anthropological record' conflates three structurally distinct readings of one persisting kernel. This file instantiates ONLY the naturalist reading as a clean epsilon-invariant constraint: the institutionalized arrangement under which the record is interpreted via materialist-scientific auspices with credentialing gates. The creationist and indigenous-epistemology readings are separate constraint stories with their own epsilon values, beneficiary/victim structures, and classifications; they are linked here as network kin because the naturalist reading's institutional dominance reshapes both siblings' legitimacy conditions and resource availability (curriculum law, funding, custody policy). Epsilon differs across the family because each reading assesses the standing arrangement by its own lights: this reading sees substantial but bounded asymmetry; a rival reading of the same referent would author materially higher extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
