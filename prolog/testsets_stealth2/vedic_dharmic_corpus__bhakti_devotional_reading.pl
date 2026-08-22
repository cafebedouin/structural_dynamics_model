% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Reading: Devotion-Based Spiritual Authority over the Vedic-Dharmic Corpus
 *   domain: religious authority / social stratification / interpretive legitimacy
 *
 * SUMMARY:
 *   The constraint under story is the devotional-access arrangement as it has
 *   actually operated across the bhakti movements: a
 *   corpus-read-as-invitation under which sincere devotion, rather than birth
 *   into a ritual lineage, determines who may teach, initiate, and stand
 *   before the divine. It opened authorized religious participation to castes
 *   the hereditary order excluded, built vernacular congregational life, and
 *   generated lineage institutions that now hold the sects' property,
 *   followings, and sincerity judgments — inside a society whose marriage,
 *   occupation, and status hierarchies the opening left standing, and which
 *   in places re-entered the devotional sphere itself (segregated seating,
 *   hereditary temple office, caste-structured successions). Epsilon's
 *   referent is this standing devotional arrangement, assessed by the
 *   reading's own lights: by those lights the opening is real and the residue
 *   — caste operating inside the sphere devotion opened — is a standing
 *   failure, which is why epsilon is moderate (0.40) rather than negligible.
 *   Family note (epsilon decomposition across the kernel): the hereditary
 *   sibling authors epsilon for the birth-monopoly arrangement (high,
 *   concentrated Brahmin gainer class); the reformist sibling authors epsilon
 *   for the same standing arrangement assessed by constitutional lights
 *   (different harmed set, authority relocated to rational critique); this
 *   story authors epsilon ~0.40 for the devotional arrangement's own
 *   operation. The three are separate constraints linked by
 *   network.affects_constraints, not one constraint measured three ways.
 *
 * KEY AGENTS:
 *   - devotional_sect_leaders: agenda-setting seat (institutional/identity_locked) — administers initiation, judges sincerity, holds lineage property; the arrangement's material flows accrue here
 *   - lower_caste_devotees: primary coordinated party (organized/constrained) — gains devotional access, gives labor and surplus, dual-positioned with a payer secondary role
 *   - dalit_outcaste_communities: primary target (powerless/trapped) — partial admission, carries the untouchability the opening promised to dissolve
 *   - women_devotees: target with opened voice (powerless/constrained) — congregational voice without office or succession rights
 *   - sectarian_donors: payer (moderate/constrained) — merchant and artisan surplus flows to the lineages
 *   - orthodox_brahmin_establishment: dispossessed-then-absorbed party (institutional/identity_locked) — lost exclusive claims, re-entered as lineage leaders and sect intellectuals
 *   - ruling_patrons: indirect gainer (institutional/arbitrage) — buys legitimation and social stability with redirectable patronage
 *   - vernacular_saint_poets: charismatic gainers (moderate/identity_locked) — authority from devotional realization, canonized by lineages that outlive them
 *   - religious_studies_scholars: analytical observer — documents the gap between the saints' anti-hierarchical verse and the lineages' stratified administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.42).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading: Devotion-Based Spiritual Authority over the Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious authority / social stratification / interpretive legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__bhakti_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, 'a392797d-129c-4a61-a168-ec2dbc612636').
narrative_ontology:cs_kernel_codification('a392797d-129c-4a61-a168-ec2dbc612636', fixed_text).
narrative_ontology:cs_authority_grounding('a392797d-129c-4a61-a168-ec2dbc612636', distributed).
narrative_ontology:cs_reading_relation('a392797d-129c-4a61-a168-ec2dbc612636', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('a392797d-129c-4a61-a168-ec2dbc612636', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('a392797d-129c-4a61-a168-ec2dbc612636', foundational, devotion_sufficient_for_authority).
narrative_ontology:cs_axiom_status(devotion_sufficient_for_authority, holdable).
narrative_ontology:cs_axiom_grounding('a392797d-129c-4a61-a168-ec2dbc612636', devotion_sufficient_for_authority, theological).
narrative_ontology:cs_axiom('a392797d-129c-4a61-a168-ec2dbc612636', foundational, divine_access_bypasses_birth_ritual_requirements).
narrative_ontology:cs_axiom_status(divine_access_bypasses_birth_ritual_requirements, holdable).
narrative_ontology:cs_axiom_grounding('a392797d-129c-4a61-a168-ec2dbc612636', divine_access_bypasses_birth_ritual_requirements, theological).
narrative_ontology:cs_reference_frame('a392797d-129c-4a61-a168-ec2dbc612636', corpus_as_devotional_invitation).
narrative_ontology:cs_drift_state('a392797d-129c-4a61-a168-ec2dbc612636', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a392797d-129c-4a61-a168-ec2dbc612636', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, vernacular_saint_poets).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_sect_leaders).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, ruling_patrons).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, dalit_outcaste_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, sectarian_donors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, orthodox_brahmin_establishment).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, orthodox_brahmin_establishment).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, divine_accessibility_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, vernacular_scriptural_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Found and head the devotional lineages: they initiate disciples across castes, authorize vernacular teaching, judge the sincerity of devotees' practice, and hold the sects' accumulated property — temple endowments, matha land, festival and donation streams. Their standing rests on the devotional principle they administer; a lineage head who abandoned it would dissolve his own authority. Succession formally runs through initiation rather than birth, though many lineages in practice favor kin.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_sect_leaders, agenda_setter,
    institutional, generational, identity_locked, continental).

% Peasant and artisan castes admitted to congregational worship, vernacular song, and initiation that the hereditary order denied them; they fill the sects' congregations and supply much of their labor and giving. What they gain in devotional standing they do not gain in marriage, occupation, or village status, which remain caste-bound; their giving sustains the lineage institutions above them, and leaving the sect would cost them the community their devotional life is built in.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_devotees, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_devotees, payer).

% Communities at the base of the caste order, whom many devotional currents admit only partially — kept from temple interiors, priestly office, or lineage succession even where their devotion is celebrated in song. They carry the untouchability the opening promised to dissolve; the bar persists across generations regardless of individual piety, and leaving the village economy that binds them is rarely possible.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, dalit_outcaste_communities, payer,
    powerless, generational, trapped, regional).

% Hold a sanctioned voice in congregational devotion — singing, composing, in some lineages leading kirtan — that orthodox ritual channels never gave them. The celebrated woman saints are exceptions whose paths required breaking household ties; the ordinary woman devotee sings in the congregation but holds no office, initiates no one, and remains subject to the family and caste arrangements that sit alongside the devotional sphere.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, women_devotees, payer,
    powerless, biographical, constrained, regional).

% Merchant and artisan households whose surplus flows to the sects as dakshina, festival sponsorship, and endowments. Giving purchases standing within the devotional community and is hard to reduce without visible loss of that standing; switching lineages is possible but carries the same obligation in a new form.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, sectarian_donors, payer,
    moderate, biographical, constrained, regional).

% Hereditary ritual scholars and temple priests whose exclusive claim on Vedic teaching and rite the devotional reading set aside. They contested the vernacular movements as authority-less, yet over generations much of the establishment moved inside the devotional world — founding and leading lineages, supplying Sanskrit learning to the sects, staffing temple ritual. Their vocation is inseparable from the scriptural order they interpret; leaving the field would dissolve the standing their learning exists to hold.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, orthodox_brahmin_establishment, payer,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, orthodox_brahmin_establishment, beneficiary).

% Kings, chieftains, and landed elites who endow devotional institutions and take legitimation back: coronation and honors from lineage heads, temple prestige, and a religious order that honors devotion across castes while leaving land, labor, and marriage hierarchies intact. Patronage is redirectable — a dynasty shifts endowment toward whichever lineage serves its standing.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, ruling_patrons, beneficiary,
    institutional, generational, arbitrage, regional).

% Composer-devotees — weavers, potters, princesses, outcaste singers — whose vernacular songs become the sects' scripture. Their authority comes from the devotional realization their verses express, not from any office they could resign; the lineages that canonize their songs outlive them and administer the access they opened, sometimes against the verse's own anti-hierarchical grain.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, vernacular_saint_poets, beneficiary,
    moderate, biographical, identity_locked, regional).

% Document and compare the movements' institutional histories: initiation economies, seating and office practice, patronage records, and the gap between the saints' anti-hierarchical verse and the lineages' stratified administration. They take no side in the devotional economy; their accounts feed revivalist and reformist movements alike.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, devotional_sect_leaders).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides authorized religious participation, scriptural meaning, and spiritual standing for the non-Brahmin majority without hereditary qualification: vernacular congregational practice, initiation open across castes, and a criterion of authority — sincere devotion — that any practitioner can in principle meet. Solves once, across the devotional sphere, the access problem the hereditary order solved only for the twice-born.
% TRANSFER_FUNCTION: Moves material surplus — dakshina, festival sponsorship, temple and matha endowments — from devotee households to devotional lineages and institutions; moves deference and interpretive obedience from devotees to lineage heads; moves legitimation and social stability to ruling patrons; moves devotional standing and community belonging to admitted devotees.
% ABSENT_VOICES: Dalit communities are present in the reading's promise ('all are equal before God') and absent from its administration: they are not in the councils that decide temple entry, priestly office, or lineage succession in the sects that admit them partially or not at all. Women hold congregational voice but not the offices that judge sincerity or transmit authority. Both would object to the gap between the saints' verse and institutional practice; their objection survives mainly inside the devotional corpus they were excluded from administering.
% DISAPPEARANCE_RATIONALE: If the devotional-access arrangement vanished overnight, the religious order would revert toward birth-qualified authority: the non-Brahmin majority's congregational and vernacular religious life would lose its authorized form, the lineage institutions holding the sects' property and followings would dissolve, and ruling patrons would lose the legitimation channel that honors devotion while leaving land and marriage hierarchies intact.
% FOUNDING_PROBLEM: The hereditary ritual monopoly left the overwhelming majority without authorized access to the sacred: non-Brahmins could not hear the Veda, perform the rites, or hold spiritual office; vernacular speakers had no scriptural legitimacy; devotion had no sanctioned path around birth qualification.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties on both ends: the hereditary order's own exclusionary texts and practice corroborate that the founding access problem was real, and the anti-caste devotional tradition itself (Kabir, Ravidas, Basava) together with dalit temple-entry movements and colonial-era reform documentation corroborates that the residue — partial admission, office denied — persists, which is why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.40 at interval end) because the arrangement genuinely opens access — the non-Brahmin majority's devotional life runs through it — while a sectarian economy has grown through the same channel: dakshina and endowment flows concentrate in lineage institutions, guru authority hardens, and the promise of devotional equality leaves the standing caste order intact outside the temple door and partly inside it. Suppression (0.42) is enforcement-shaped rather than state-shaped: initiation control, sincerity judgment, and lineage discipline do the holding, and the series shows that machinery maturing as the sects acquired property and patronage. Theater (0.30) is low-to-moderate: congregational devotion is mostly functionally sincere; the performative share grows with institutional ritual and guru charisma but does not dominate. Accessibility_collapse (0.45) is mid-range because the devotional reading does not close alternatives — hereditary practice continues beside it and reformist critique emerges after it — though within a devotee's life, embeddedness in a lineage makes exit costly. Resistance (0.40) reflects orthodox contestation of vernacular authority and internal anti-caste critique of the sects' compromises. All three tracked series run on one shared grid (t=0,6,12,18,24,30) so no metric is sampled against another's end-state. The claim is authored from structure — genuine coordination and asymmetric payment running through the same devotional channel, held by active lineage enforcement — and the metrics from the arrangement's observed operation; where the engine's per-seat computation diverges from the tangled_rope claim, that divergence is data, not error. The manifest's hypothesis ('coordination rope rather than snare') is honored as a ruling-out of snare; refinement to tangled_rope follows from the non-empty harmed set the manifest itself declares ('victim set shrinks but does not eliminate caste hierarchy') plus the active lineage enforcement that holds the channel. Boltzmann coordination type is identity_coordination because the dominant function is membership: who may claim devotional standing, adjudicated against evolving criteria (sincerity rather than birth); the type's conservative floor is left at default, and the omegas route the risk that identity framing covers extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the lineage head's seat the arrangement is the access revolution it stewards: doors it opened, followings it serves. From the lower-caste devotee's seat it is opened worship inside unchanged social standing — real access, real giving, hierarchy intact outside the congregation. From the dalit seat it is partial admission: celebrated in song, barred from the interior and the office. From the orthodox establishment's seat it began as dispossession and became absorption — its learning staffs the very lineages that set its exclusive claims aside. The engine computes these per-seat classifications from power, exit, and declared position; the divergence between the agenda-setter's and the trapped payer's computed experience is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put admitted devotees, saint-poets, lineage heads, and patrons at the low-d end; victim declarations put dalit communities, women devotees, and donor households at the high-d end, with exit options modulating within each group: trapped, powerless dalit communities sit nearest the full-target end; arbitrage-grade patrons (redirectable patronage) sit nearest the beneficiary end among the gainers. The dual-role declarations carry the tangle: lower-caste devotees are beneficiaries with a payer secondary role (access and giving through the same channel), and the orthodox establishment is a payer with a beneficiary secondary role (dispossessed, then absorbed into lineage leadership). The derivation reads both from the declared structure, so no directionality overrides are needed. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled, by directionality and by the arrangement's regional-to-continental scopes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the majority's exclusion from authorized access — is not dead: vernacular devotional access remains the operative religious path for the communities it opened, so the arrangement's mandate has not outlived its function and no mandatrophy resolution is declared. What has partially atrophied is the reading's egalitarian edge: the anti-caste verse is canonized while the lineages that canonize it administer caste-structured institutions. The tangled_rope classification prevents the two standard mislabelings: reading the arrangement as pure coordination would erase the documented sectarian economy and the consolation function — spiritual equality absorbing the energy of equality claims while land, labor, and marriage hierarchies stand; reading it as pure extraction would erase the access revolution that the majority's religious life actually runs through and that dalit and women devotees still press to complete. The classification holds both the opening and the take, and the omega set routes the open question — whether the sincerity standard is an opening or a new gate — to resolvable evidence rather than resolving it by label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (bhakti_devotional_reading) of the vedic_dharmic_corpus kernel; what structural differences would the sibling readings (hereditary_monopoly_reading, reformist_egalitarian_reading) produce for the same corpus?',
    'Comparative classification across the three reading-stories linked by network.affects_constraints: each authors its own epsilon, beneficiary set, and victim set for the standing arrangement under contest.',
    'The hereditary sibling concentrates the gainer class in the Brahmin lineage and raises epsilon sharply; the reformist sibling shifts the harmed set toward holders of traditional authority and lowers epsilon. The disagreement is located in the criterion of spiritual authority — birth, devotion, or constitutional conformity — not in the corpus itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the corpus governs, and how the criterion-of-authority choice restructures who gains and who bears costs.').

omega_variable(
    sincerity_gatekeeping_function,
    'Does the sincerity standard function as the access-opening the reading intends, or has assessment of sincerity hardened into a new gate administered by devotional lineages?',
    'Comparative study of initiation (diksha) practice, exclusion cases, and who may authorize teaching across sampradayas over the interval.',
    'If sincerity-assessment has hardened into lineage-controlled gate functionally parallel to birth qualification, effective extraction rises and the reading drifts toward the structure it opposed; if assessment remains open and revisable, the moderate extraction estimate stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_gatekeeping_function, empirical, 'Whether the sincerity criterion opens access or replaces birth qualification with a new gate.').

omega_variable(
    consolation_vs_catalysis,
    'Does the devotional-equality promise defuse pressure for social equality (absorbing the energy of equality claims while the caste order stands) or catalyze it (devotional dignity converting into social and economic claims)?',
    'Historical-comparative analysis of caste-mobility and anti-caste mobilization trajectories in regions of strong devotional institutionalization versus regions where devotional currents remained anti-institutional.',
    'If consolation dominates, the arrangement draws from lower-caste devotees by absorbing equality claims, raising epsilon above 0.40; if catalysis dominates, the devotional sphere functions as a staging ground for equality and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consolation_vs_catalysis, empirical, 'Whether spiritual democratization defuses or feeds social democratization.').

omega_variable(
    restratification_by_own_lights,
    'By the bhakti reading''s own lights, is the re-stratification of its institutions (segregated seating, hereditary temple office, caste-structured successions) a condemned drift that raises the arrangement''s extraction, or a permitted accommodation that leaves it moderate?',
    'Doctrinal analysis of how sampradaya authorities themselves classify caste accommodation — condemned compromise, provisional concession, or positive dharma.',
    'If the reading''s own authorities condemn the re-stratification, epsilon assessed by the reading''s lights is higher than 0.40 and the drift is unacknowledged; if they endorse it as accommodation, 0.40 stands but the reading''s egalitarian premise is narrower than its saints'' verse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restratification_by_own_lights, conceptual, 'Whether caste accommodation inside devotional institutions violates or fulfills the reading''s own standard.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression that keeps devotees within stratified devotional practice structural (initiation control, institutional gatekeeping, patron dependence) or internalized (karma-and-dharma cosmology under which hierarchy reads as natural and deserved)?',
    'Post-exit suppression trajectory: track devotees who leave lineages or join anti-caste movements — if deference to devotional hierarchy persists after the institutional mechanism is removed, part of the suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure — the cosmology travels with the devotee after exit — and the reading''s opening is shallower than participation rates suggest; if structural, institutional reform could release it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in stratified devotional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bhakti_devotional_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(bhakti_devotional_tr_t0, observed).
narrative_ontology:measurement(bhakti_devotional_tr_t6, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement_basis(bhakti_devotional_tr_t6, observed).
narrative_ontology:measurement(bhakti_devotional_tr_t12, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(bhakti_devotional_tr_t12, observed).
narrative_ontology:measurement(bhakti_devotional_tr_t18, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(bhakti_devotional_tr_t18, observed).
narrative_ontology:measurement(bhakti_devotional_tr_t24, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(bhakti_devotional_tr_t24, observed).
narrative_ontology:measurement(bhakti_devotional_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(bhakti_devotional_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(bhakti_devotional_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(bhakti_devotional_be_t0, observed).
narrative_ontology:measurement(bhakti_devotional_be_t6, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement_basis(bhakti_devotional_be_t6, observed).
narrative_ontology:measurement(bhakti_devotional_be_t12, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement_basis(bhakti_devotional_be_t12, observed).
narrative_ontology:measurement(bhakti_devotional_be_t18, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement_basis(bhakti_devotional_be_t18, observed).
narrative_ontology:measurement(bhakti_devotional_be_t24, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement_basis(bhakti_devotional_be_t24, observed).
narrative_ontology:measurement(bhakti_devotional_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(bhakti_devotional_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(bhakti_devotional_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(bhakti_devotional_su_t0, observed).
narrative_ontology:measurement(bhakti_devotional_su_t6, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 6, 0.16).
narrative_ontology:measurement_basis(bhakti_devotional_su_t6, observed).
narrative_ontology:measurement(bhakti_devotional_su_t12, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 12, 0.24).
narrative_ontology:measurement_basis(bhakti_devotional_su_t12, observed).
narrative_ontology:measurement(bhakti_devotional_su_t18, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 18, 0.31).
narrative_ontology:measurement_basis(bhakti_devotional_su_t18, observed).
narrative_ontology:measurement(bhakti_devotional_su_t24, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement_basis(bhakti_devotional_su_t24, observed).
narrative_ontology:measurement(bhakti_devotional_su_t30, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(bhakti_devotional_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Vedic-dharmic authority structure' covers three structurally distinct claims that must not share one story: (1) authority by birth into Brahmin lineage (hereditary sibling — high epsilon, concentrated gainer class), (2) authority by sincere devotion (this story — moderate epsilon, diffuse gains, shrunk but non-empty harmed set), (3) textual meaning conformed to constitutional equality (reformist sibling — different harmed set, authority relocated to rational critique). The epsilon values differ because each reading assesses the standing dharmic arrangement by its own lights; they form a constraint family rather than one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
