% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Orthodox Literalist Dharmasastra Authority (Eternal Revealed Varna/Jati Order)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The orthodox literalist reading holds the Dharmasastra corpus — above all
 *   its varna/jati prescriptions — to be eternal, revealed truth requiring
 *   literal observance, and organizes social life around that holding:
 *   hereditary occupation, endogamous marriage, graded purity, and male
 *   twice-born ritual authority. The tradition presents the arrangement as
 *   natural and self-evidently ordered (a mountain-shaped claim); the
 *   structural record shows active enforcement, suppressed exits,
 *   identifiable victims, and concentrated beneficiaries. The claim/metric
 *   gap is deliberate and is the measurement this corpus exists to take: the
 *   orthodox framing claims eternal natural law, while the authored metrics
 *   describe enforced, extractive operation. KEY AGENTS (by structural
 *   relationship): brahmin_priestly_class — agenda-setting
 *   interpreter-beneficiary (institutional/arbitrage), transmits the corpus
 *   and collects its rents; kshatriya_ruling_elites — enforcing beneficiary
 *   (powerful/constrained); vaishya_commercial_castes — ranked beneficiary
 *   (organized/constrained); shudra_laboring_castes — primary target
 *   (powerless/trapped); dalits_untouchable_communities — most intensive
 *   target (powerless/trapped); hindu_women_under_patriarchy — cross-cutting
 *   target present in every rank (powerless/trapped); dalit_rights_advocates
 *   — excluded dissenter (organized/mobile); indological_scholars —
 *   analytical observer (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.78).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.55).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.78).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Orthodox Literalist Dharmasastra Authority (Eternal Revealed Varna/Jati Order)").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, 'a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a').
narrative_ontology:cs_kernel_codification('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', fixed_text).
narrative_ontology:cs_authority_grounding('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', lineage).
narrative_ontology:cs_interpretation_layer_present('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a').
narrative_ontology:cs_reading_relation('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', foundational, varna_eternal_revealed_order).
narrative_ontology:cs_axiom_status(varna_eternal_revealed_order, holdable).
narrative_ontology:cs_axiom_grounding('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', varna_eternal_revealed_order, theological).
narrative_ontology:cs_axiom('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', foundational, svadharma_literal_observance_binding).
narrative_ontology:cs_axiom_status(svadharma_literal_observance_binding, holdable).
narrative_ontology:cs_axiom_grounding('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', svadharma_literal_observance_binding, deontological).
narrative_ontology:cs_axiom('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', secondary, karma_birth_status_theodicy).
narrative_ontology:cs_axiom_status(karma_birth_status_theodicy, holdable).
narrative_ontology:cs_axiom_grounding('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', karma_birth_status_theodicy, theological).
narrative_ontology:cs_reference_frame('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', apaurusheya_eternal_varnashrama).
narrative_ontology:cs_drift_state('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', contemporary_constitutional_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a40d7281-a3dd-4fe6-b1e1-cc7255b0ad2a', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_ruling_elites).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaishya_commercial_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits_untouchable_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_laboring_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, hindu_women_under_patriarchy).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, apaurusheya_revelation_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, karma_rebirth_theodicy).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, svadharma_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Memorize, transmit, and adjudicate the corpus; alone perform Vedic ritual and teach the Vedas; receive dakshina fees, ritual gifts, and hereditary service entitlements from households and patrons across the varna order. Their families' standing, marriage alliances, and livelihoods are constituted by the arrangement they interpret. Across the interval they have moved between royal courts, temple establishments, and modern professions while retaining interpretive authority; leaving the fold means forfeiting the status and livelihood the arrangement assigns them.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class, beneficiary).

% Hold land, arms, and political command; their rule is consecrated by the corpus, which obligates the other orders to serve and obey them. They patronize priests, endow temples, and enforce caste discipline through state power. Their legitimacy travels with the arrangement, so abandoning it would strip their rule of its sanction.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_ruling_elites, beneficiary,
    powerful, generational, constrained, regional).

% Trade, lend, and hold merchant wealth under a rank that places them above all laboring and formerly untouchable communities. They finance ritual life and marry within ranked networks. They bear deference obligations toward the two higher orders but collect precedence, creditworthiness, and marriage capital from standing above everyone below them.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaishya_commercial_castes, beneficiary,
    organized, biographical, constrained, regional).

% Perform agricultural labor and hereditary service for the twice-born; barred from Vedic recitation and formal learning; taxed and obligated to render service; marriage confined within jati. Exit historically meant losing caste altogether — expulsion from kin, occupation, and ritual community — so generations remained in assigned station.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_laboring_castes, payer,
    powerless, biographical, trapped, continental).

% Handle work the corpus codes as polluting — scavenging, leatherwork, corpse disposal — and live segregated outside village bounds; denied entry to temples, schools, and wells; subject to ritual deference demands whose breach invites violence. Conversion offered an exit but at the price of community, livelihood, and stigma that followed them across religious lines.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits_untouchable_communities, payer,
    powerless, biographical, trapped, continental).

% Across all varnas, women are assigned lifelong dependence — on fathers, then husbands, then sons — barred from Vedic study and independent ritual agency, married young within caste, and bound to chastity and widow-austerity norms. Upper-caste women share their household's rank while carrying these disabilities; lower-caste women carry rank-subordination and gender-subordination together. For most of the interval, exit meant destitution or death.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, hindu_women_under_patriarchy, payer,
    powerless, biographical, trapped, continental).

% Anti-caste thinkers, Ambedkarite organizers, and leaders of converted communities contest the arrangement's authority and document its harms from lived experience. The orthodox interpretive establishment classifies their testimony as heterodox and admits none of them to the seats where the corpus's meaning is settled.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_rights_advocates, excluded,
    organized, generational, mobile, national).

% Date the corpus's strata, compare manuscripts, and reconstruct the historical conditions of its redaction. They describe how prescriptions changed across recensions and how enforcement varied by polity and period, supplying the evidentiary record that all disputing parties cite.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, indological_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns every person a hereditary occupation, marriage pool, and ritual station, organizing labor specialization, purity management, and succession of social roles across generations without renegotiation.
% TRANSFER_FUNCTION: Moves ritual authority, access to sacred learning, honor and precedence, and surplus labor and service from Shudra, Dalit, and female members of society to the twice-born male hierarchy, concentrating recurring material flows in the Brahmin priestly class.
% ABSENT_VOICES: Those the arrangement ranks lowest — Dalit communities, Shudra laborers, and women — had no seat in the interpretive conversation that produced and policed the corpus. Their objection survives only in traditions outside the orthodox frame (bhakti poets, anti-caste movements), which the orthodox reading classifies as heterodox rather than admissible testimony.
% DISAPPEARANCE_RATIONALE: If the orthodox literalist arrangement vanished overnight, marriage markets, occupational inheritance, village ritual economies, purity etiquette, and the entire deference order between jatis would rearrange; the priestly class would lose its revenue and monopoly, landholding and merchant elites would lose consecrated legitimacy, and the excluded labor of Dalits and the domestic and reproductive subordination of women would lose their sanctioning framework.
% FOUNDING_PROBLEM: Consolidating diverse kinship-based communities and occupational groups into a stable social order under a shared ritual framework in the post-Vedic period — fixing who may teach, sacrifice, rule, trade, serve, and marry whom.
% FOUNDING_PROBLEM_CORROBORATION: No corroborator outside the beneficiary set attests the orthodox claim that the prescriptions are eternal revelation; that attestation comes only from the tradition's own authorized transmitters. Outside it, the manuscript record shows the corpus accreting and revising across strata in response to concrete historical contests, and non-Brahmin sources — Buddhist polemic, anti-caste testimony, colonial-era ethnography — attest that the arrangement consolidated elite authority rather than answering a general social need. Whether the underlying problem (ordering a complex society) remains live in some form is disputed between the orthodox party and its critics.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end; peaking at 0.90 before constitutional dismantling) because the arrangement transfers learning, ritual authority, honor, labor, and service upward by birth, with the transfer rate set by the beneficiaries themselves. Suppression (0.55 currently) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled in the engine's computation. The suppression series tracks enforcement capacity, which is the dynamic this story traces: royal and customary enforcement hardened through the medieval period, peaked under late-precolonial codification, then fell sharply after constitutional abolition of untouchability and legal equality measures — enforcement decay, not voluntary relaxation. Theater rises steadily (0.10 to 0.38) as legal enforcement collapsed and observance became partly declarative: public profession of orthodoxy decoupling from practiced discrimination. Accessibility collapse is 0.7 — alternatives (conversion, reform, migration) never fully collapsed, which is why resistance (0.7) is high and persistent: bhakti dissent, anti-caste movements, mass conversion. Coalition potential among the powerless is real but fragmented by design: the graded rank of the hierarchy gives every jati someone below it, so the arrangement's gradient is itself a coalition-suppression mechanism. All three metric series run on one shared time grid (200, 600, 1200, 1750, 1947, 2026) so no metric is sampled against another's end-state. The boltzmann type is declared identity_coordination because the arrangement's dominant coordination function is membership and boundary maintenance; the conservative 0.08 floor is diagnostically appropriate here precisely because identity framing ('this is our eternal dharma') is the arrangement's own cover story.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from identical structural data. From the Brahmin interpreter's position the arrangement is stewardship: a sacred trust transmitted through lineage, in which collecting dakshina is remuneration for preserving civilization. From the Shudra, Dalit, and female seats the same structure operates as enforced extraction: learning denied, labor owed, deference compelled, exit punished. Kshatriya and Vaishya beneficiaries experience it as legitimate order that happens to place them mid-rank. The engine computes this per-seat divergence from power, exit, and directional data; the authored snare claim does not adjudicate it — it records the authoring seat's structural judgment that the enforcement-and-victim profile dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class sits nearest the beneficiary end (collects dakshina, service entitlements, and the learning monopoly; demonstrated arbitrage-grade adaptivity across regimes while retaining position). Kshatriya and Vaishya seats are beneficiaries with declining magnitude down the rank. Shudra and Dalit seats sit nearest the full-target end: trapped exit, hereditary station, no arbitrage. Women cut across every rank — beneficiaries of household rank where born high, targets of the gender prescriptions everywhere — so their derived directionality is high but not maximal, and the cross-cutting is documented in the situation text rather than forced into a single override. The excluded and observer seats lie outside the beneficiary/victim derivation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Vedic consolidation of a stable social order under shared ritual authority) is contested: the orthodox party holds dharma-ordering eternally live; the critical record holds the specific consolidation long superseded. The mismatch consumer should read this carefully: this is NOT a zombie case. A piton reading would require that the arrangement persist by inertia while nobody profits enough to maintain it — flatly false here, since the beneficiary concentration is the point and the gains flow to a nameable seat. Conversely, a pure-extraction reading must not erase the genealogy: the arrangement did once solve a real coordination problem, and the reformist sibling reading exists precisely because that residue is arguable. Classifying this reading as a snare keeps the victims visible and the enforcement dependency explicit, while the founding-problem interview preserves the historical coordination function from being retroactively denied or retroactively exculpatory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_construction,
    'Is the varna/jati hierarchy a natural or cosmic order (gunakarma-determined, revealed, self-enforcing — the orthodox mountain-claim) or a constructed arrangement maintained by identifiable beneficiaries?',
    'Comparative textual history demonstrating redaction and accommodation across recensions; cross-cultural comparison showing no equivalent hierarchy emerging where the corpus did not travel; mobility data from periods and regions of weak enforcement.',
    'If constructed, the eternal-law framing collapses and the arrangement stands as enforced policy serving named beneficiaries; if the orthodox claim survived scrutiny, the constraint would approach natural-law certification and the victim analysis would require reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_construction, empirical, 'Natural-law claim versus constructed-hierarchy reality of the varna order.').

omega_variable(
    kernel_reading_structural_delta,
    'How would the classification of this arrangement change under the sibling readings of the dharmasastra_corpus kernel — reformist_contextual (ethical core separable from time-bound caste prescription) and abolitionist_rejection (no legitimate authority remains)?',
    'Author the sibling stories as separate constraints and compare victim sets, epsilon, and computed types; the disagreement is located in the status of the prescriptions themselves (eternally binding versus historically conditioned versus wholly illegitimate).',
    'The reformist reading shrinks the victim set (those harmed only by caste prescription drop out) and lowers epsilon; the abolitionist reading removes legitimate authority entirely, rendering the arrangement pure historical extraction with no coordination residue. This story''s figures are valid only for the orthodox_literalist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed classification: this story is one reading of a contested kernel, not the topic whole.').

omega_variable(
    enforcement_attribution_ambiguity,
    'How much of the observed suppression belongs to the textual-literalist arrangement itself, versus to local jati panchayats, state power, and economic dependency that might persist under a contextualist reading?',
    'Compare periods and regions where textual orthodoxy weakened but caste discipline persisted (non-Brahminical polities, diaspora communities) against regions where enforcement followed the corpus''s institutional reach.',
    'Determines whether epsilon attributes extraction to the literalist reading specifically or to caste as a broader formation; high attribution to non-textual mechanisms would lower this story''s epsilon and shift weight to sibling stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_attribution_ambiguity, empirical, 'Attribution of suppression between the textual arrangement and its carriers.').

omega_variable(
    internalized_suppression_mechanism,
    'Is the suppression keeping lower castes and women in place primarily structural (violence, economic dependency, legal disability, segregation) or internalized (karma-theodicy making subordination self-understood as deserved)?',
    'Post-exit suppression trajectory: track communities that converted or migrated — if deference norms, purity anxiety, and status acceptance persist after the enforcement mechanism is removed, reclassify a substantial share as internalized.',
    'If heavily internalized, effective suppression exceeds the structural measure and outlives enforcement removal, explaining why legal abolition (1947-1955) produced slower behavioral change than the enforcement-decline series alone predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Structural versus internalized share of the measured suppression.').

omega_variable(
    svadharma_fulfillment_claim,
    'Do the orthodox apologia — that each varna finds duty, purpose, and spiritual fulfillment in svadharma — constitute genuine benefit flowing to lower seats, or rationalized cover for extraction?',
    'Preference-sensitive analysis: survey expressed valuation of station by members of lower varnas inside versus outside enforcement contexts, weighting revealed behavior (exit attempts, conversion, resistance) over stated contentment under enforcement.',
    'If fulfillment claims register as genuine benefit for some seats, part of the measured extraction converts to coordination cost and the beneficiary structure becomes partially symmetric; if they track enforcement intensity, they are cover and the extraction figure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(svadharma_fulfillment_claim, preference, 'Whether svadharma fulfillment is benefit or rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 200, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(dhar_tr_t200, observed).
narrative_ontology:measurement(dhar_tr_t600, dharmasastra_corpus__orthodox_literalist, theater_ratio, 600, 0.12).
narrative_ontology:measurement_basis(dhar_tr_t600, observed).
narrative_ontology:measurement(dhar_tr_t1200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1200, 0.18).
narrative_ontology:measurement_basis(dhar_tr_t1200, observed).
narrative_ontology:measurement(dhar_tr_t1750, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1750, 0.22).
narrative_ontology:measurement_basis(dhar_tr_t1750, observed).
narrative_ontology:measurement(dhar_tr_t1947, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1947, 0.3).
narrative_ontology:measurement_basis(dhar_tr_t1947, observed).
narrative_ontology:measurement(dhar_tr_t2026, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2026, 0.38).
narrative_ontology:measurement_basis(dhar_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 200, 0.72).
narrative_ontology:measurement_basis(dhar_be_t200, observed).
narrative_ontology:measurement(dhar_be_t600, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 600, 0.78).
narrative_ontology:measurement_basis(dhar_be_t600, observed).
narrative_ontology:measurement(dhar_be_t1200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1200, 0.86).
narrative_ontology:measurement_basis(dhar_be_t1200, observed).
narrative_ontology:measurement(dhar_be_t1750, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1750, 0.9).
narrative_ontology:measurement_basis(dhar_be_t1750, observed).
narrative_ontology:measurement(dhar_be_t1947, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1947, 0.85).
narrative_ontology:measurement_basis(dhar_be_t1947, observed).
narrative_ontology:measurement(dhar_be_t2026, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(dhar_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 200, 0.68).
narrative_ontology:measurement_basis(dhar_su_t200, observed).
narrative_ontology:measurement(dhar_su_t600, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 600, 0.74).
narrative_ontology:measurement_basis(dhar_su_t600, observed).
narrative_ontology:measurement(dhar_su_t1200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1200, 0.82).
narrative_ontology:measurement_basis(dhar_su_t1200, observed).
narrative_ontology:measurement(dhar_su_t1750, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1750, 0.87).
narrative_ontology:measurement_basis(dhar_su_t1750, observed).
narrative_ontology:measurement(dhar_su_t1947, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1947, 0.62).
narrative_ontology:measurement_basis(dhar_su_t1947, observed).
narrative_ontology:measurement(dhar_su_t2026, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(dhar_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Dharmasastra authority' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. The orthodox_literalist story (this file) carries the expansive victim set and enforced-hierarchy extraction; the reformist_contextual story carries a narrowed victim set (only what survives separating ethics from caste prescription) and lower epsilon; the abolitionist_rejection story carries no legitimate authority and pure historical extraction. The orthodox reading is the historical upstream: its claims are what the reformist reading cites and modifies and what the abolitionist reading rejects wholesale. Each file links the others via network.affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
