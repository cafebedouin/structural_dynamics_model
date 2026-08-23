% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Reading — Divine Mandate of the Fourfold Order
 *   domain: religious/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   The orthodox varna reading holds that the Vedic corpus — supremely the
 *   Purusha Sukta's cosmogony of the four orders carved from a primordial
 *   body — literally prescribes a divinely mandated social hierarchy, with
 *   outcaste laborers positioned outside the body altogether. This story
 *   instantiates that reading as a constraint in its own right (kernel:
 *   vedic_corpus_social_prescription; reading: orthodox_varna_reading),
 *   modeling the standing arrangement the reading asserts and administers: a
 *   hereditary division of labor, marriage, and ritual access enforced
 *   through purity discipline, religious sanction, and community coercion.
 *   Per the epsilon-invariance principle this is ONE of three sibling
 *   constraints decomposed from the contested colloquial label of what the
 *   Vedas say about caste: the reformist_spiritual_reading (spiritual unity,
 *   metaphorical cosmology, no prescriptive social content) and the
 *   colonial_orientalist_reading (a unified, timeless Hindu law system
 *   codifiable for administration) are separate stories with their own
 *   epsilon values, victim sets, and classifications, linked through
 *   network.affects_constraints. The claim/metric gap is deliberate and
 *   substantive: the reading CLAIMS cosmic-natural status — its
 *   self-description is a naturality claim, eternal and divinely ordained —
 *   while the authored metrics describe actively enforced extraction with
 *   identifiable victims; the divergence between that claim and the computed
 *   classification is exactly the false-summit signal this corpus exists to
 *   take. Interval mapping: one time unit is approximately five years, T0
 *   corresponds to roughly 1785 CE (late pre-colonial regime) and T48 to
 *   roughly 2025 CE.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_caste: agenda-setter and principal beneficiary (institutional/identity_locked) — authors, transmits, and interprets the corpus; collects ritual fees, land-grant revenue, and labor service; controls ritual access
 *   - kshatriya_ruling_elites: secondary beneficiary and co-enforcer (powerful/constrained) — receives legitimated dominion, pays tribute in revenue grants and ritual subordination
 *   - vaishya_merchant_agrarian_castes: dual-positioned (organized/mobile) — finances the ritual economy, bears the tax burden, receives contractual stability
 *   - shudra_laboring_castes: primary target (powerless/trapped) — supplies agricultural and artisanal labor, barred from Vedic study, station inherited by children
 *   - dalit_outcaste_communities: primary target (powerless/trapped) — placed outside the order entirely; their exclusion defines the purity of every rank above
 *   - heterodox_ascetic_traditions: excluded voice (organized/constrained) — recorded the contemporaneous objection that liberation requires no hierarchy
 *   - comparative_historians_of_religion: analytical observer (analytical/analytical) — reconstructs composition history and reception across communities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.74).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.58).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Reading — Divine Mandate of the Fourfold Order").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/hermeneutics/social_stratification").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '3be209f0-2985-4cdc-ba4e-5d3673f3cc9a').
narrative_ontology:cs_kernel_codification('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', fixed_text).
narrative_ontology:cs_authority_grounding('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', lineage).
narrative_ontology:cs_interpretation_layer_present('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a').
narrative_ontology:cs_reading_relation('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', foundational, varna_hierarchy_is_divine_injunction).
narrative_ontology:cs_axiom_status(varna_hierarchy_is_divine_injunction, holdable).
narrative_ontology:cs_axiom_grounding('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', varna_hierarchy_is_divine_injunction, theological).
narrative_ontology:cs_axiom('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', secondary, caste_duty_transgression_incurs_spiritual_demerit).
narrative_ontology:cs_axiom_status(caste_duty_transgression_incurs_spiritual_demerit, holdable).
narrative_ontology:cs_axiom_grounding('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', caste_duty_transgression_incurs_spiritual_demerit, theological).
narrative_ontology:cs_reference_frame('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', eternal_varnashrama_cosmic_order).
narrative_ontology:cs_drift_state('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', contemporary_constitutional_repudiation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3be209f0-2985-4cdc-ba4e-5d3673f3cc9a', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_elites).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laboring_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_outcaste_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_agrarian_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_agrarian_castes).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, purusha_sukta_cosmogonic_charter).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, svadharma_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Memorize, transmit, and interpret the corpus; alone officiate the rites that license marriage, death, and succession; decide who may study the Veda. Receive dakshina fees, agrahara land grants whose revenue is alienated from rulers, and customary labor service from dependent villages. Occupy the top of the purity ranking their own rulings police; their schooling, marriage alliances, and livelihood all presuppose the order they interpret.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste, beneficiary).

% Hold military and administrative power and enforce the varna duties of their subjects; receive consecration and legitimation from priestly officiants as the price of sovereign standing. Cede revenue through land grants to priests and temples, and rank ritually below the priesthood they empower. Rejecting the order would forfeit the legitimacy that constitutes their rule.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_elites, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_elites, agenda_setter).

% Farm, trade, and lend under the order's protection; finance temples and festivals to purchase standing. Bear heavy taxation and ritual subordination — barred in orthodox practice from Vedic study — while benefiting from enforceable contracts and stable commercial networks spanning the subcontinent. Wealth occasionally converts into higher ceremonial standing.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_agrarian_castes, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_merchant_agrarian_castes, payer).

% Supply agricultural labor and artisanal service to the twice-born orders; barred from Vedic study and from most ritual agency. Bound to patrons through debt and custom; their children inherit occupation and station. Flight or conversion costs community, marriage network, and livelihood at once.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_laboring_castes, payer,
    powerless, generational, trapped, regional).

% Live outside the varna order altogether, in settlements at the village edge; confined to occupations ranked as polluting — scavenging, leatherwork, corpse handling. Denied entry to temples, access to wells, and schooling; transgression draws boycott and violence. Their exclusion is the boundary that defines the purity of every rank above them.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_outcaste_communities, payer,
    powerless, generational, trapped, local).

% Teach rival paths — Buddhist and Jain ordination, later devotional movements — that bypass priestly mediation and admit all castes. Their canons record the objection that liberation requires no social hierarchy. Tolerated at the margins, absorbed as subordinate sects, or displaced; their testimony survives as the main contemporaneous counter-record.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, heterodox_ascetic_traditions, excluded,
    organized, civilizational, constrained, continental).

% Reconstruct the corpus's composition across strata and centuries, trace how a cosmogonic hymn became a legal code, and compare how different communities received the same texts. Take no part in the order they study.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, comparative_historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large agrarian civilization's division of labor, marriage network, and ritual calendar across generations without centralized administrative machinery: hereditary occupational assignment reduces recurring negotiation over who does what, and a shared purity-and-duty vocabulary links dispersed local communities into one transregional order while organizing knowledge transmission through priestly lineages.
% TRANSFER_FUNCTION: Moves labor service, agricultural surplus, ritual fees, land-grant revenue, and deference upward from Shudra and outcaste laborers to Brahmin and Kshatriya elites; moves legitimation downward from the priesthood to rulers who enforce the order.
% ABSENT_VOICES: The Shudra and outcaste laborers themselves — whose consent was never solicited and whose counter-reading (that their labor is taken, not owed) was systematically unrecorded because they were excluded from literacy, assembly, and the interpretive tradition. Heterodox teachers and lower-caste devotional poets objected and were marginalized or absorbed; women of all varnas were barred from textual authority. Their objection survives mainly in fragments: heterodox canons, Tamil bhakti poetry, and later reformist and anti-caste testimony.
% DISAPPEARANCE_RATIONALE: Occupational roles would be renegotiated rather than inherited, marriage pools would reorganize beyond endogamous caste lines, ritual authority would be contested openly, and land-labor relations built on hereditary servitude would restructure — the social architecture of the civilization depended on the arrangement holding.
% FOUNDING_PROBLEM: Stabilizing a post-Rigvedic pastoral-agrarian society's division of labor and integrating diverse kinship groups into a single ritual-political order — accomplished by chartering an inherited hierarchy as cosmic structure: the Purusha Sukta's dismembered primordial body assigns social functions as parts of a divinity, making contingent power appear as eternal order.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox parties attest the founding problem eternally live (dharma is held to be timeless). Outside the beneficiary set, Buddhist and Jain canonical literature attests the hierarchy was contested from the era of its consolidation; bhakti poets record rejection of birth-ranked ritual access; modern philology attests the rigid fourfold scheme consolidated gradually in the sutra and Dharmashastra strata rather than descending intact from the earliest hymns; Ambedkarite and Dalit intellectual testimony attests the arrangement long outlived any organizing function it once served. Corroboration exists, and it divides — hence contested.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end, peaking at 0.88 during colonial-era codification) because the arrangement transfers labor, surplus, and ritual income upward while confining the payers to hereditary stations — the transfer is decoupled from any service the beneficiaries render to those who pay it. Suppression (0.58 end-state, down from a 0.84 colonial-era peak) is real but decayed: formal state enforcement of caste disability ended with the 1950 constitution, leaving decentralized social coercion — boycott, violence against transgressors, matrimonial policing — to carry the load; the suppression_requirement series is authored because enforcement capacity is precisely what changed over this interval. Theater ratio rises steadily (0.20 to 0.44) as legal enforcement collapsed and maintenance shifted toward performative assertion: purity display, symbolic hierarchy at weddings and festivals, public defense of tradition increasingly detached from the extractive functions it once organized. Accessibility collapse is moderate-high (0.72): conversion, urban migration, and secular employment exist but cost community, marriage network, and often livelihood. Resistance (0.55) is persistent and documented — Buddhist and Jain dissent, bhakti egalitarianism, anti-caste movements, constitutional politics — and is met with backlash rather than accommodation. Victim numerical superiority has repeatedly failed to convert into dissolution: sub-caste gradations among the oppressed and differential proximity to patron seats fragment coalitions (see the victim_coalition_capacity omega). Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream. All three metric series share one nine-point grid so the engine samples every metric at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the brahmin seat the arrangement is sacred order: meaningful, identity-constituting, experienced as service to dharma rather than as taking — the engine should compute a coordination-flavored type from that seat's structural position. From the shudra and dalit seats the same structure is totalizing extraction dressed as cosmic necessity: labor taken, ritual agency denied, exit punished. The vaishya seat sits between, buying stability with surplus. The observer seat sees the mechanism itself: a naturality claim doing the work of suppressing scrutiny of who pays. The brahmin seat's identity lock is institutional-professional fusion — status, learning, marriage, and livelihood are all constituted by the interpretive office, so breaking the frame would cost the seat everything it is, which is why the seat defends the reading even where its material position would survive reform. The authored claim does not adjudicate among these seats; the engine computes per-seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: brahmin_priestly_caste and kshatriya_ruling_elites sit near the beneficiary pole; shudra_laboring_castes and dalit_outcaste_communities, declared victims with trapped exit, sit near the full-target pole. One override is warranted: kshatriya_ruling_elites derive a deep-beneficiary directionality from their beneficiary declaration alone, but they pay real tribute into the same structure — revenue alienated through priestly land grants, ritual fees, and legitimation dependency — so the override sets d to 0.32 for the powerful atom, marking partial capture rather than pure subsidy. Vaishya castes are left to derivation: their dual beneficiary/payer position lands mid-scale, matching their buy-stability-with-surplus situation. Scope gradients amplify effective extraction asymmetrically: the priestly collection network operates at continental scale where verification is hardest, while outcaste exclusion is enforced at hyper-local scale where escape is hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against mountain: the reading's self-description is a naturality claim — divinely mandated cosmic order — and taking that description at face value would certify an unchangeable natural law; the structural data refute it (identifiable beneficiaries, coerced victims, active enforcement, a documented history of resistance and reform), which is why emerges_naturally is authored false despite the reading's own assertion. Against rope: the arrangement does coordinate a genuine division of labor, but the coordination story functions as cover — the same structure that organizes labor transfers its product upward and punishes exit, the snare signature rather than mutual-benefit coordination. On mandatrophy: the founding problem (stabilizing a post-Rigvedic society's division of labor and integrating kinship groups into one ritual-political order) is contested — orthodox parties attest it eternally live, anti-caste and reformist parties attest it long dead as justification while the arrangement persists on interest, inertia, and belief. Because the status is contested rather than plainly dead, no mandatrophy resolution is declared; the mismatch consumer reads the contested-status x world_rearranges combination directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel vedic_corpus_social_prescription — the orthodox_varna_reading holding that the corpus literally prescribes the varna hierarchy as divine mandate. Would the reformist_spiritual_reading (no prescriptive social content) or the colonial_orientalist_reading (administrable Hindu law) instantiate a structurally different constraint?',
    'Philological analysis of genre distribution across corpus strata (samhita, brahmana, sutra, dharmashastra) plus reception history: which communities at which periods treated which strata as binding social prescription.',
    'If the reformist reading is textually better supported, this constraint''s victim set reflects institutional enforcement of a contested interpretation rather than textual design, and its classification migrates from snare toward a hermeneutically contested construct; if the orthodox reading tracks the dominant historical reception, the snare classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the literal-prescription reading is the corpus''s operative meaning or one contestable interpretation among siblings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression holding Shudra and Dalit communities in place structural (economic dependency, violence, legal disability) or internalized (karma-dharma ideology making the inherited station feel deserved)?',
    'Post-legal-equality compliance trajectories: track occupational and marital transgression rates after the 1950 constitutional barriers fell; sustained low transgression despite reduced coercion indicates internalized suppression.',
    'If substantially internalized, effective suppression exceeds the structural measure and the constraint persists after enforcement decay; classification drifts toward deeper entrapment than the exit-option atoms suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism in caste compliance.').

omega_variable(
    stratal_attribution_of_prescription,
    'Does the rigid varna prescription originate in the Vedic samhitas themselves (the Purusha Sukta, RV 10.90) or in later ritual sutras and Dharmashastra texts retrojecting authority onto Vedic antiquity?',
    'Philological dating and comparative analysis: occupational flexibility in Rigvedic and later-Vedic narrative passages versus the fixed fourfold scheme''s first systematic appearance in the later sutra literature.',
    'If the prescription is largely post-Vedic retrojection, the reading''s antiquity-and-naturality claim collapses further, confirming constructed status and strengthening the snare reading over any residual mountain framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stratal_attribution_of_prescription, empirical, 'Which textual stratum actually carries the rigid prescriptive content.').

omega_variable(
    belief_vs_material_interest_persistence,
    'Does the reading''s contemporary persistence flow from continued doctrinal belief or from material interests (land, labor, marriage capital) operating behind doctrinal language?',
    'Compare behavior of economically emancipated upper-caste actors who publicly reject the doctrine: if matrimonial and alliance practices remain endogamous despite professed rejection, material interest dominates.',
    'If material interest dominates, dismantling requires economic restructuring rather than hermeneutic refutation; the constraint would persist under any successor reading that preserves existing asset positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(belief_vs_material_interest_persistence, empirical, 'Doctrinal belief versus material interest as the driver of persistence.').

omega_variable(
    victim_coalition_capacity,
    'Can the numerically dominant victim castes convert majority position into coalition power sufficient to dissolve the constraint, or do fragmentation (sub-caste gradations among the oppressed) and differential proximity to patron seats block coalition?',
    'Comparative analysis of anti-caste mobilization episodes: where cross-caste coalitions formed, what dissolved them — absorption, repression, or internal status competition.',
    'If coalition capacity is structurally blocked, the arrangement is stable indefinitely despite victim numerical superiority; if buildable, its persistence depends on active divide-and-maintain enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_capacity, empirical, 'Whether victim-class coalition power is a live threat to the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcsp_ovr_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t0, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t6, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t6, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t12, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t12, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t18, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t18, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t24, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t24, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t30, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t30, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t36, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t36, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t42, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 42, 0.4).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t42, observed).
narrative_ontology:measurement(vcsp_ovr_tr_t48, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 48, 0.44).
narrative_ontology:measurement_basis(vcsp_ovr_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(vcsp_ovr_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(vcsp_ovr_be_t0, observed).
narrative_ontology:measurement(vcsp_ovr_be_t6, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 6, 0.79).
narrative_ontology:measurement_basis(vcsp_ovr_be_t6, observed).
narrative_ontology:measurement(vcsp_ovr_be_t12, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement_basis(vcsp_ovr_be_t12, observed).
narrative_ontology:measurement(vcsp_ovr_be_t18, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 18, 0.85).
narrative_ontology:measurement_basis(vcsp_ovr_be_t18, observed).
narrative_ontology:measurement(vcsp_ovr_be_t24, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 24, 0.88).
narrative_ontology:measurement_basis(vcsp_ovr_be_t24, observed).
narrative_ontology:measurement(vcsp_ovr_be_t30, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 30, 0.87).
narrative_ontology:measurement_basis(vcsp_ovr_be_t30, observed).
narrative_ontology:measurement(vcsp_ovr_be_t36, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 36, 0.8).
narrative_ontology:measurement_basis(vcsp_ovr_be_t36, observed).
narrative_ontology:measurement(vcsp_ovr_be_t42, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 42, 0.77).
narrative_ontology:measurement_basis(vcsp_ovr_be_t42, observed).
narrative_ontology:measurement(vcsp_ovr_be_t48, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 48, 0.74).
narrative_ontology:measurement_basis(vcsp_ovr_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(vcsp_ovr_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(vcsp_ovr_su_t0, observed).
narrative_ontology:measurement(vcsp_ovr_su_t6, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 6, 0.73).
narrative_ontology:measurement_basis(vcsp_ovr_su_t6, observed).
narrative_ontology:measurement(vcsp_ovr_su_t12, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement_basis(vcsp_ovr_su_t12, observed).
narrative_ontology:measurement(vcsp_ovr_su_t18, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 18, 0.82).
narrative_ontology:measurement_basis(vcsp_ovr_su_t18, observed).
narrative_ontology:measurement(vcsp_ovr_su_t24, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement_basis(vcsp_ovr_su_t24, observed).
narrative_ontology:measurement(vcsp_ovr_su_t30, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement_basis(vcsp_ovr_su_t30, observed).
narrative_ontology:measurement(vcsp_ovr_su_t36, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(vcsp_ovr_su_t36, observed).
narrative_ontology:measurement(vcsp_ovr_su_t42, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 42, 0.63).
narrative_ontology:measurement_basis(vcsp_ovr_su_t42, observed).
narrative_ontology:measurement(vcsp_ovr_su_t48, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(vcsp_ovr_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label of what the Vedas say about caste conflates three structurally distinct claims. This story (orthodox_varna_reading) authors the literal-divine-prescription constraint: high epsilon, Shudra/Dalit victim set, Brahmin capture. The reformist_spiritual_reading authors a no-prescriptive-content constraint whose victim set dissolves into interpretive error. The colonial_orientalist_reading authors a codification-for-governance constraint whose victims are those administered under codified caste law. Upstream/downstream structure: this reading influenced the orientalist reading (orthodox pandit interpretation supplied the textual authority and content that colonial codification consumed), and colonial codification in turn hardened this arrangement's enforcement (visible in the measurement series' colonial-era peak). All three stories link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
