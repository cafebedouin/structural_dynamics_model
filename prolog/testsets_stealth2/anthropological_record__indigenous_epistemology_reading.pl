% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Relational-Continuity Reading of the Anthropological Record (Oral-Tradition Governance)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested
 *   anthropological_record kernel: the claim that the record reveals
 *   relational continuity between living communities and their ancestors and
 *   places, knowable through sustained oral tradition. As an operative
 *   arrangement, that claim vests authority over ancestral remains in
 *   descendant communities, requires that material evidence be read together
 *   with transmitted knowledge, and seats credentialed and scriptural
 *   frameworks in subordinate positions. The claim and the metrics are
 *   authored independently: the claimed type states what I judge structurally
 *   true of this arrangement from the reading's own seat, while the metrics
 *   describe its observed operation — including a slow rise in procedural
 *   theater that the claim does not paper over. Sibling readings (naturalist,
 *   creationist) are separate constraint files, not folded into this one; the
 *   kernel contest is routed to omegas and kernel_context per the
 *   committer-frame rules.
 *
 * KEY AGENTS:
 *   - descendant_communities: primary beneficiary (organized/identity_locked) — receive custody and interpretive authority; the relationship is constitutive, not optional
 *   - oral_tradition_keepers: secondary beneficiary (moderate/identity_locked) — testimony gains formal standing; carry transmission discipline
 *   - archaeological_researchers: cost-bearing participant with incidental gains (organized/constrained) — lose unilateral access, gain interpretive information
 *   - museum_collections_institutions: cost-bearing participant (institutional/constrained) — bear inventory, repatriation, and deaccession costs
 *   - heritage_regulators: agenda setter (institutional/constrained) — administer and enforce the statutory framework
 *   - scriptural_origin_advocates: excluded voice (organized/mobile) — contest for standing from outside the governance tables
 *   - philosophy_of_science_observers: analytical seat (analytical/analytical) — document the epistemics without holding stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.15).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.28).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Relational-Continuity Reading of the Anthropological Record (Oral-Tradition Governance)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '06d8cdbe-7489-4c47-aebd-cfea95c2ca81').
narrative_ontology:cs_kernel_codification('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', distributed).
narrative_ontology:cs_authority_grounding('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', lineage).
narrative_ontology:cs_interpretation_layer_present('06d8cdbe-7489-4c47-aebd-cfea95c2ca81').
narrative_ontology:cs_reading_relation('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', anthropological_record__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', anthropological_record__creationist_reading, influences).
narrative_ontology:cs_axiom('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', foundational, oral_tradition_authoritative_not_supplementary).
narrative_ontology:cs_axiom_status(oral_tradition_authoritative_not_supplementary, holdable).
narrative_ontology:cs_axiom_grounding('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', oral_tradition_authoritative_not_supplementary, empirically_contingent).
narrative_ontology:cs_axiom('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', foundational, community_authority_supersedes_credentialed_and_scriptural_frameworks).
narrative_ontology:cs_axiom_status(community_authority_supersedes_credentialed_and_scriptural_frameworks, holdable).
narrative_ontology:cs_axiom_grounding('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', community_authority_supersedes_credentialed_and_scriptural_frameworks, deontological).
narrative_ontology:cs_reference_frame('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', relational_continuity_living_lineage).
narrative_ontology:cs_drift_state('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', contemporary_post_repatriation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('06d8cdbe-7489-4c47-aebd-cfea95c2ca81', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, descendant_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, oral_tradition_keepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, archaeological_researchers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, archaeological_researchers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, museum_collections_institutions).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, relational_continuity_doctrine).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, oral_tradition_evidentiary_standing).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, ancestral_custody_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and transmit accounts of origin, migration, and kinship that tie living members to specific ancestors and places. Seek custody of ancestral remains held in distant institutions and a decisive voice in how sites are excavated, interpreted, and taught. Participation runs through consultation bodies, excavation permits, and repatriation requests. Stepping outside the arrangement is not a live option: the relationships it recognizes are constitutive of who the community is.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, descendant_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Elders and appointed knowledge-holders who carry narratives, songs, and place-names across generations and give testimony in consultations, courts, and curricula. The arrangement gives their testimony formal standing alongside material finds; they bear the discipline of accurate transmission and answer to their communities for it.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, oral_tradition_keepers, beneficiary,
    moderate, generational, identity_locked, local).

% Excavate, date, and publish on human remains and occupation sites. Under the arrangement they must consult before fieldwork, accept community decisions over disturbance and analysis of ancestors, and often co-design projects with community researchers. They lose unilateral access and speed; they gain interpretive information such as place-names, event accounts, and landscape knowledge that material evidence alone does not carry. Careers built on unrestricted access feel the loss most sharply; collaborative practice opens other paths.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, archaeological_researchers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, archaeological_researchers, beneficiary).

% Universities, museums, and government repositories holding ancestral remains and cultural items gathered during colonial-era collecting expeditions. They must inventory holdings, publish notices, and return ancestors and items on request; storage, staffing, and deaccession are recurring costs and study collections shrink. Many now fund community-run curation and joint exhibitions; a minority litigated to retain holdings.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, museum_collections_institutions, payer,
    institutional, generational, constrained, national).

% Statutory offices that administer repatriation and burial-protection law: certifying community heritage programs, setting consultation standards, mediating disputed claims, and penalizing institutions that miss deadlines. They drafted and now maintain the rules the other seats operate under.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, heritage_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Communities and organizations who read deep-human-history questions through scriptural timelines or design arguments. They campaign through school boards, museum boards, and legislatures for equal standing of their accounts. The governance tables over ancestral remains do not include them; their filings and amicus briefs arrive from outside.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, scriptural_origin_advocates, excluded,
    organized, generational, mobile, national).

% Philosophers of science, historians of anthropology, and anthropologists of knowledge who study how testimony, material evidence, and institutional authority interact in this domain. They publish on evidential pluralism and the ethics of collection; they hold no custody and bear no compliance costs.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, philosophy_of_science_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, descendant_communities).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the governance of ancestral remains, burial sites, and interpretive authority among descendant communities, researchers, and holding institutions: who may access, disturb, study, move, and speak for the record, with sustained oral tradition required as the interpretive companion to material evidence.
% TRANSFER_FUNCTION: Moves custodial authority and interpretive standing from credentialed institutions and individual researchers to descendant communities; moves compliance costs (inventory, repatriation, consultation) onto holding institutions; moves formal evidentiary recognition to oral tradition keepers.
% ABSENT_VOICES: Scriptural-origin advocates and strict-materialist researchers who reject community decision rights over evidence are outside the governance tables; they speak from litigation, legislatures, and adjacent disciplines. Within communities, members who dissent from particular transmitted accounts or particular custodial decisions may also lack a recognized seat, depending on each community's internal governance.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, holding institutions would resume unilateral control of ancestral remains, oral tradition would lose its formal evidentiary standing, consultation regimes would collapse, and the conflict structure of the mid-twentieth century — injunctions, protests over excavations, standoffs over collections — would return within a season.
% FOUNDING_PROBLEM: Colonial-era collecting took ancestral remains and sacred items without consent, severed communities from ancestors and places, and dismissed transmitted knowledge as myth — producing at once a justice problem (desecration and custody) and an epistemic problem (a material record stripped of the interpretive key that travels with it).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legislative findings written into repatriation statutes attest the history of expropriation; museum-profession associations have formally acknowledged past collection practices; court records from retention litigation document the contested holdings; and non-Indigenous historians of anthropology have published the collecting-era record independently.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).
:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.15): the arrangement's principal movement is restorative — authority and ancestors flow back toward the communities they were taken from — and the residual extraction is coordination overhead (consultation time, compliance process) rather than rent. Suppression (0.28) is a raw structural property, unscaled by power or scope: it reflects legally backed limits on unilateral excavation and study, coercion that is bounded, aimed at institutions rather than communities, and surrounded by lawful alternatives (collaborative designs, other regions, other questions). Theater (0.30) is low but rising: consultation is drifting toward checkbox performance in some jurisdictions, the story's main Goodhart watch item. Accessibility collapse is moderate (0.45): the strict-materialist alternative does not vanish — it persists outside the governed domain and inside collaborative practice — but within the governed domain the community-authority rule binds and purely unilateral study is closed off. Resistance is moderate (0.42): retention litigation and disciplinary grumbling were real, and have declined as collaboration normalized. The three measurement series share one seven-point grid (1968–2026); the suppression_requirement series traces enforcement-capacity maturation — statutes passed, agencies staffed, penalties applied — rising through the statutory era and plateauing at the scalar value, not an intensifying ratchet; the extractiveness series declines as ad hoc friction gave way to settled procedure, with a slight recent uptick from proceduralization; the theater series rises monotonically as documented above.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From the community and knowledge-holder seats the arrangement restores what was taken and validates a knowledge system — a subsidized position with near-zero felt extraction. From the museum and researcher seats the same structure removes long-standing prerogatives and imposes recurring costs — a constrained position that will compute as materially more extractive. The regulator seat experiences administration, not sacrifice. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Descendant communities and oral tradition keepers sit near the beneficiary end: custody, standing, and recognition flow to them, and their identity-lock means no exit discount applies. Museum institutions and archaeological researchers bear the transfer's costs — compliance burdens, lost access — placing them toward the target end despite their lack of victim status in this reading's accounting; researchers recover part of the cost through interpretive gains unavailable from material evidence alone, which moderates their effective position. Heritage regulators administer the arrangement and collect no rents. Scriptural advocates are excluded rather than coordinated — outside the flow entirely. Regional and national scopes keep verification difficulty moderate, so scope amplification of effective extraction is modest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: repatriation backlogs remain large, custody disputes continue, and dismissal of transmitted knowledge persists in some venues — so the arrangement's mandate has not outlived its function and no mandatrophy is declared. The classification guards against two misreadings in opposite directions: it prevents labeling the arrangement as pure extraction (its dominant movement is restitution, and its beneficiaries are the parties from whom things were taken), and it prevents reading the enforcement build-up as repression (the coercive force aims at institutional retention of ancestors, not at the communities). The rising theater ratio is the genuine decay vector to monitor: if consultation becomes fully ceremonial while custody decisions concentrate elsewhere, the arrangement would begin migrating toward inertial performance — a drift the temporal series is positioned to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the anthropological_record kernel: what structurally differs if the naturalist_reading or creationist_reading is instantiated instead?',
    'Classify the sibling stories separately and compare epsilon, victim sets, and authority seats; track over time which reading actually governs custody decisions, exhibit content, and funding.',
    'If the naturalist reading governs, community authority over remains weakens, oral tradition loses evidentiary standing, and this constraint''s enforcement decays toward the pre-statutory arrangement; if the creationist reading governs, scriptural authority replaces community authority with a different beneficiary set and a different evidentiary economy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which reading of the kernel is instantiated and what siblings would change.').

omega_variable(
    oral_tradition_corroboration_rate,
    'How often do testable claims carried by sustained oral tradition — sea-level change, volcanic events, species ranges, migration routes — agree with independently dated material archives?',
    'Systematic paired studies comparing transmitted accounts against geological, palaeoecological, and archaeological dating in the same locales.',
    'High agreement strengthens this reading against the naturalist sibling; but if the reading''s authority were made to depend wholly on agreement rates, counterevidence would erode it — the deontological custody axiom is what insulates authority from that route.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_corroboration_rate, empirical, 'Empirical standing of transmitted knowledge against material archives.').

omega_variable(
    authority_ground_independence,
    'Is community authority over ancestral remains grounded in the evidential superiority of transmitted knowledge, or in relational obligation that holds regardless of evidential status?',
    'Examine community legal testimony and internal doctrine: whether custody claims are argued from duty to ancestors or from accuracy of accounts; observe conduct when transmitted accounts and material evidence conflict.',
    'If duty-grounded, the constraint is evidence-insulated and the naturalist sibling cannot displace it by improving method; if evidence-grounded, the two readings compete on shared terrain and corroboration outcomes decide between them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_ground_independence, conceptual, 'Locates the precise structural element on which this reading and the naturalist sibling disagree.').

omega_variable(
    speaking_for_the_community,
    'When transmitted accounts differ within a descendant community, whose version governs custody and interpretation decisions?',
    'Document each community''s own governance structures for adjudicating internal disagreement, and how heritage regulators treat competing claimants from the same community.',
    'Determines whether the arrangement''s authority concentrates in particular knowledge-holder lineages or distributes across the community, and therefore whose directionality the constraint effectively subsidizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaking_for_the_community, conceptual, 'Internal-representation ambiguity in the beneficiary seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1968, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1968, 0.08).
narrative_ontology:measurement(anth_tr_t1978, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(anth_tr_t1988, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1988, 0.13).
narrative_ontology:measurement(anth_tr_t1998, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(anth_tr_t2008, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2008, 0.23).
narrative_ontology:measurement(anth_tr_t2018, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(anth_tr_t2026, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(anth_be_t1968, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1968, 0.22).
narrative_ontology:measurement(anth_be_t1978, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1978, 0.19).
narrative_ontology:measurement(anth_be_t1988, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1988, 0.17).
narrative_ontology:measurement(anth_be_t1998, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1998, 0.15).
narrative_ontology:measurement(anth_be_t2008, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2008, 0.14).
narrative_ontology:measurement(anth_be_t2018, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2018, 0.15).
narrative_ontology:measurement(anth_be_t2026, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2026, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1968, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1968, 0.08).
narrative_ontology:measurement(anth_su_t1978, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1978, 0.14).
narrative_ontology:measurement(anth_su_t1988, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1988, 0.22).
narrative_ontology:measurement(anth_su_t1998, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1998, 0.26).
narrative_ontology:measurement(anth_su_t2008, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2008, 0.27).
narrative_ontology:measurement(anth_su_t2018, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2018, 0.28).
narrative_ontology:measurement(anth_su_t2026, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the anthropological record' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story family sharing the kernel: this reading (relational continuity via sustained oral tradition, community authority over remains), the naturalist reading (materialist origins via scientific method, method-based institutional authority), and the creationist reading (divine creation compatible with scriptural timeline or designed complexity, scriptural authority). Each story carries its own epsilon, beneficiary/victim structure, and classification; this reading's epsilon is assessed by its own lights over the standing arrangement it instantiates. Edges run to both siblings: the naturalist reading currently supplies the institutional upstream (collections, funding, curricula) that this reading partially displaces, and this reading's institutionalization exerts structural pressure on the creationist reading's heritage-governance ambitions without resolving the underlying dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
