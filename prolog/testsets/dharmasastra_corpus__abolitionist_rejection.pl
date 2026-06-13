% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus as Hierarchical Extraction (Abolitionist Reading)
 *   domain: religious_law/normative_authority/textual_interpretation
 *
 * SUMMARY:
 *   The Dharmasastra corpus—Sanskrit texts prescribing social, legal, ritual,
 *   and moral order in ancient and medieval Hindu civilization—is read here
 *   as fundamentally and irredeemably extractive. From this abolitionist
 *   perspective, the texts encode a hierarchical system (varna/jati caste
 *   framework, Brahmanical ritual authority, patriarchal family law) designed
 *   to extract labor, surplus, and deference from lower castes and women
 *   while legitimizing that extraction as cosmic truth and righteous duty.
 *   The abolitionist reading denies any separability of an 'ethical core'
 *   from the hierarchical prescriptions; it holds that the entire framework
 *   is the mechanism of oppression and retains zero legitimate authority.
 *   This is one of three competing readings of the same textual kernel: the
 *   orthodox-literalist reading claims eternal binding authority; the
 *   reformist-contextual reading seeks to salvage ethical principles by
 *   contextualizing caste as time-bound; the abolitionist reading rejects the
 *   text's authority entirely.
 *
 * KEY AGENTS:
 *   - Brahmanical priesthood: institutional beneficiary, maintains textual authority and ritual gatekeeping
 *   - Upper-varna landholders: powerful beneficiary, extract labor and surplus legitimized by hierarchy
 *   - Dalit communities: powerless payer, structurally excluded from personhood and learning
 *   - Shudra communities: moderate payer, bound to service occupations without property rights
 *   - Women across castes: moderate payer, subject to patriarchal restrictions and educational exclusion
 *   - Religious minorities: excluded, pressured to accept Dharmasastra categories despite non-membership
 *   - Reformist scholars: excluded observer, critique the reading but presuppose Dharmasastra retains authority
 *   - Orthodox authorities: excluded institutional opposition, defend Dharmasastra as eternal truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.89).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.91).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.89).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus as Hierarchical Extraction (Abolitionist Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/normative_authority/textual_interpretation").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '75ce1648-0690-4af6-aea0-99666430f4ed').
narrative_ontology:cs_kernel_codification('75ce1648-0690-4af6-aea0-99666430f4ed', fixed_text).
narrative_ontology:cs_authority_grounding('75ce1648-0690-4af6-aea0-99666430f4ed', extraction).
narrative_ontology:cs_interpretation_layer_present('75ce1648-0690-4af6-aea0-99666430f4ed').
narrative_ontology:cs_reading_relation('75ce1648-0690-4af6-aea0-99666430f4ed', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('75ce1648-0690-4af6-aea0-99666430f4ed', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_axiom('75ce1648-0690-4af6-aea0-99666430f4ed', foundational, dharmasastra_fundamentally_oppressive).
narrative_ontology:cs_axiom_status(dharmasastra_fundamentally_oppressive, holdable).
narrative_ontology:cs_axiom_grounding('75ce1648-0690-4af6-aea0-99666430f4ed', dharmasastra_fundamentally_oppressive, empirically_contingent).
narrative_ontology:cs_axiom('75ce1648-0690-4af6-aea0-99666430f4ed', foundational, caste_hierarchy_not_eternal_law).
narrative_ontology:cs_axiom_status(caste_hierarchy_not_eternal_law, holdable).
narrative_ontology:cs_axiom_grounding('75ce1648-0690-4af6-aea0-99666430f4ed', caste_hierarchy_not_eternal_law, empirically_contingent).
narrative_ontology:cs_reference_frame('75ce1648-0690-4af6-aea0-99666430f4ed', eternal_cosmic_varna_dharma).
narrative_ontology:cs_drift_state('75ce1648-0690-4af6-aea0-99666430f4ed', post_colonial_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('75ce1648-0690-4af6-aea0-99666430f4ed', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahmanical_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, upper_varna_landholders).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_across_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, religious_minorities).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, scriptural_texts_as_power_instruments).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__abolitionist_rejection, hierarchy_naturalization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains interpretive authority over Dharmasastra texts and ritual protocols. Controls access to learning, certification of Vedic knowledge, and religious legitimacy. Enforces the hierarchical framework through textual authentication and ritual gatekeeping. Their social position, economic support through donations and land grants, and institutional continuity depend entirely on the text's authority remaining unquestioned.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahmanical_priesthood, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% Accumulate agricultural surplus and labor obligation from lower castes justified by Dharmasastra prescriptions of dharma-based duty. Their wealth, governance authority, and social status are legitimized and enforced through the hierarchical framework. They control enforcement mechanisms (village councils, exclusion from water sources, denial of commerce) that maintain caste boundaries.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, upper_varna_landholders, beneficiary,
    powerful, generational, arbitrage, regional).

% Designated as ritually polluting, excluded from temples, water sources, education, and most occupations. Perform obligatory labor (sanitation, leather work, agricultural service) without reciprocal support. Face violent enforcement of boundary maintenance. Their humanity is structurally denied through the text's framework; exit would require abandoning all social structure and kinship simultaneously.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_communities, payer,
    powerless, biographical, trapped, regional).

% Bound to service occupations without property rights or learning access; taxed and bound to provide labor and goods to upper castes. Some Shudra subgroups (farmers, merchants) accumulate modest wealth but remain subject to sumptuary restrictions and exclusion from religious authority. Their labor is the primary extraction mechanism; exit requires abandoning caste identity entirely.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_communities, payer,
    moderate, biographical, constrained, regional).

% Subject to Dharmasastra prescriptions restricting property ownership, divorce, remarriage, and religious participation regardless of caste. Bound to patriarchal household authority and restricted from learning Sanskrit or accessing textual knowledge directly. Their productive and reproductive labor is appropriated; their legal personhood is mediated through male guardians. Exit is conflated with loss of kinship identity and social death.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_across_castes, payer,
    moderate, biographical, identity_locked, regional).

% Not mentioned in Dharmasastra texts; occupy liminal or stigmatized positions in the framework. Pressured to adopt Hindu identity and caste placement or face marginalization and exclusion from economic participation. Their religious frameworks are ignored in legal and administrative contexts; their objections to the Dharmasastra system lack institutional voice.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, religious_minorities, excluded,
    moderate, biographical, constrained, regional).

% Argue that Dharmasastra contains separable ethical principles that can be salvaged through reinterpretation. They propose contextualizing caste as time-bound rather than eternal. Their moderate critique is excluded from this reading's framework because it presupposes that Dharmasastra retains any legitimate authority — the abolitionist reading denies this presupposition entirely.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, reformist_scholars, excluded,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, reformist_scholars, observer).

% Claim Dharmasastra prescriptions are eternal, revealed truth binding all Hindus. They actively suppress abolitionist and reformist readings through institutional authority and textual counter-arguments. From this reading's perspective, their claims are the mechanism by which extraction and suppression are legitimized and perpetuated.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, orthodox_literalist_authorities, excluded,
    institutional, civilizational, identity_locked, regional).

% Examines the constraint from outside the commitment framework — neither defending nor defending against Dharmasastra authority. Observes how the constraint operates, how different readings distribute harm and benefit, and how the framework's legitimacy depends on suppression of alternatives.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, brahmanical_priesthood).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None from the abolitionist reading's perspective: what appears as coordination in reformist or orthodox framings (defining social roles, establishing ritual protocols, allocating labor) is reframed here as the machinery of extraction itself. The apparent 'coordination problem' (how to organize society) is solved by denying personhood and agency to entire classes — not a genuine coordination solution but its inversion.
% TRANSFER_FUNCTION: Moves labor (agricultural, domestic, ritual, and manual service work), land-derived surplus, ritual services, and legal deference from lower castes and women to Brahmanical priesthood and upper-varna landholders. Moves also dignity, learning access, and human status from victims to beneficiaries, all legitimized through scriptural authority.
% ABSENT_VOICES: Dalit communities and women across castes are structurally excluded from textual interpretation and religious authority. Their objections to the framework are not heard within institutional contexts; their own ethical frameworks and alternative social visions are not recorded in the texts. Religious minorities' non-participation is ignored in the normative structure. Reformist voices questioning the text's eternal status are suppressed by orthodox authority.
% DISAPPEARANCE_RATIONALE: If Dharmasastra textual authority and the caste system it legitimizes vanished overnight, the entire system of labor obligation, ritual hierarchy, land distribution, and marriage law would need reconstruction. Social hierarchy would not disappear, but this specific form — justified through eternal scriptural truth — would collapse. Brahmanical priesthood would lose institutional authority and economic support. Dalit and Shudra communities would gain legal personhood and property rights. Women would no longer be bound to guardianship and restricted learning. The organizational principle of society would fundamentally shift.
% FOUNDING_PROBLEM: Dharmasastra texts were composed to solve the administrative and social problem of a hierarchical agricultural civilization: how to organize labor, ritual, property, and kinship across diverse populations with differentiated roles. The texts claim to solve this through eternal cosmic law (varna as cosmic principle, not human invention) and righteous duty (dharma as binding obligation across hierarchy).
% FOUNDING_PROBLEM_CORROBORATION: The abolitionist reading holds that the founding problem is not only solved but solved by the modern state's legal framework (secular property law, labor contracts, constitutional personhood). Multiple voices from outside the Dharmasastra beneficiary set attest this: Dalit scholars and liberation theologians argue the text's administrative function is obsolete and its extractive function is all that remains; modern legal theory shows secular alternatives that do not require hereditary hierarchy; historical analysis demonstrates that pre-Dharmasastra societies organized labor and kinship through different mechanisms. The Brahmanical priesthood and orthodox scholars contest this status — they claim the founding problem (maintaining cosmic order, ensuring proper dharma) remains eternally live. This disagreement is precisely the contested status codified here.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the constraint operates through denial of basic human status to large populations. Every major prescriptive element (caste hierarchy, Brahmanical monopoly on learning and ritual, women's legal incompetence, untouchability of Dalit labor) serves extraction directly — not as a side effect but as the primary function. Suppression is equally high (0.91) because the system depends entirely on preventing exit and foreclosing alternatives: Dalit escape via conversion or migration faces violent enforcement; women's exit via learning or claiming authority faces institutional and kinship-level suppression; questioning the text's authority faces religious and social ostracism. Theater ratio is moderate (0.42) because significant portions of the framework DO involve real administrative functions (dispute resolution, property allocation, ritual coordination), but these are inextricably bound to extraction — the administrative machinery cannot be cleanly separated from the oppressive hierarchy it instantiates. Accessibility collapse is high (0.87) because the texts present the hierarchy as eternally binding cosmic law, not human choice — alternatives are rendered not merely difficult but unthinkable within the framework itself. Resistance is substantial (0.68) because victim groups have continuously opposed the framework (Dalit social movements, female reform movements, conversion movements, modern abolitionist readings) even at great cost.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute radically different types from identical structural data. From the Brahmanical priesthood's position, Dharmasastra is genuine coordination and righteous cosmic law — rope or even mountain from that seat. From the Dalit seat, it is pure extraction defended by violence and false naturalization — snare. From the reformist scholar's seat, it is tangled rope (containing both coordination and extraction, separable through reinterpretation). The abolitionist reading anchors to the victim-seat perspective and goes further: it denies that any frame from within Dharmasastra's own logic can capture what is structurally true. The divergence is not a bug but the measurement the classification system is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmanical priesthood: d ≈ 0.05 (full beneficiary — controls interpretation, collects donations, maintains social position without coercion). Upper-varna landholders: d ≈ 0.10 (strong beneficiary — extract surplus, control enforcement, have arbitrage options via conversion or mobility). Dalit communities: d ≈ 0.98 (near-total target — trapped by violence and social death, extraction of all labor-related produce, denial of learning and civic participation, high accessibility collapse due to spiritual legitimization). Shudra communities: d ≈ 0.82 (strong target — constrained exit, obligatory service, but some economic development possible, lower accessibility collapse than Dalit communities). Women across castes: d ≈ 0.85 (strong target — identity-locked to patriarchal household, educational exclusion, property incompetence, but not ritual pollution like Dalits; the constraint operates across all castes). The directionality override is unnecessary here — the structural derivation tracks the abolitionist analysis exactly.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy issue arises here because the founding problem is classified as dead from the outset. The abolitionist reading does not attempt to rescue Dharmasastra as a coordination solution; it explicitly denies that it ever was one. The extraction itself IS the disagreement: the beneficiaries claim the hierarchy solves a live problem (cosmic order, proper dharma); the victims and abolitionists claim the 'problem' is constructed by the texts themselves and solved by dismantling the framework. This is exactly where a contested founding_problem_status lands — not a hidden or gradually-revealed mandatrophy, but an explicit and irreconcilable disagreement about whether the text's function remains legitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dharmasastra_as_historical_product_vs_eternal_truth,
    'Are Dharmasastra texts products of specific historical circumstances (post-Vedic administrative needs, particular class interests, responses to economic organization challenges), or do they encode eternal cosmic law binding all Hindus across time?',
    'Historical-comparative analysis of textual evolution, Sanskrit philology tracking composition dates and layers, comparative examination of caste hierarchies across cultures and eras to test whether varna system is universal or culturally specific. Examination of alternative Hindu frameworks (Bhakti traditions, non-caste Buddhist/Jain alternatives that coexisted) as evidence that caste was not the only available model.',
    'If texts are historical products, the abolitionist reading is strengthened: they can be rejected as obsolete and replaced. If the axiom of eternality holds within any major interpretive school, the orthodoxliteralist reading retains institutional authority. If the texts contain both layers — some eternal, some historical — the reformist reading''s separability claim gains ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dharmasastra_as_historical_product_vs_eternal_truth, empirical, 'Whether Dharmasastra is historical construction or eternal truth').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression measured (0.91) primarily structural (economic exclusion, violent enforcement, institutional gatekeeping) or significantly internalized (victims have internalized shame and believed their lower status is deserved, carry suppression even after structural barriers are removed)?',
    'Ethnographic and psychological studies tracking Dalit and women''s communities after structural barriers are removed (via law, migration, or institutional reform) to measure whether suppression persists. Comparison of communities that escaped the Dharmasastra framework (via conversion to Christianity, Islam, Buddhism, or modern secular law) to measure long-term suppression trajectories. Oral history of liberation movements documenting the point at which internal psychological barriers were overcome.',
    'If suppression is primarily structural, removal via legal reform and alternative frameworks is tractable. If internalized, the cost of complete recovery is higher and longer — the framework''s suppressive force persists even after external mechanisms are dismantled. The measurement remains 0.91 either way, but the trajectory of change differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism').

omega_variable(
    dharmasastra_beneficiary_consciousness,
    'Do beneficiaries (Brahmanical priesthood, upper-varna landholders) consciously maintain Dharmasastra authority as a tool for extraction, or do they sincerely believe in the cosmic truth and righteousness of the hierarchy?',
    'Textual analysis of Sanskrit commentaries and treatises for explicit arguments about cosmic order, beneficiary testimony from different eras (more transparent in modern debates where secular vocabulary is unavailable as cover), examination of behavior when the framework is contested (do beneficiaries defend it as truth or admit its instrumental character when pressed).',
    'If beneficiaries are conscious instrumentalists, the extraction is clearer and easier to demonstrate. If they are sincere believers, the abolitionist analysis must contend with the fact that oppression can be sustained through false consciousness rather than deliberate malice. Either way, the harm is identical — the framework must be dismantled — but the roadmap for change differs (institutional deceit vs. ideological capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharmasastra_beneficiary_consciousness, empirical, 'Beneficiary consciousness of instrumentality vs. sincere belief').

omega_variable(
    alternative_social_organization_viability,
    'Can complex hierarchical societies be organized without the Dharmasastra framework or something functionally similar? Is caste-hierarchy-without-legitimizing-text actually possible, or do hierarchies naturally re-emerge even when the scriptural justification is removed?',
    'Historical comparison with societies that abolished caste-like frameworks (modern India''s constitutional law, countries that abolished hereditary class systems, comparative examination of caste elimination in converted communities). Long-term monitoring of communities where Dharmasastra authority is rejected to measure whether non-caste hierarchies re-emerge. Theoretical analysis of whether hierarchical labor division requires scriptural legitimation or whether it can persist through pure structural coercion.',
    'If alternative organization is viable, the abolitionist reading is pragmatically defensible — a new framework can replace Dharmasastra. If hierarchies re-emerge inevitably, the abolitionist reading faces a harder problem: what structural prevention of hierarchy is possible? The constraint might shift from snare (Dharmasastra-based) to a different snare (economic coercion without scriptural cover, or renewed caste-like hierarchies). This does not invalidate abolition, but it complicates the victory condition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_social_organization_viability, conceptual, 'Viability of non-hierarchical or non-caste-based social organization').

omega_variable(
    reformist_reading_coexistence_vs_foreclosure,
    'Can the reformist-contextual reading (ethical core separable from time-bound caste prescriptions) coexist with the abolitionist reading in the same interpretive space, or does accepting one reading logically foreclose the other?',
    'Logical analysis of whether a party can hold both ''Dharmasastra contains separable ethical principles'' (reformist) and ''Dharmasastra retains zero legitimate authority'' (abolitionist) without contradiction. Historical examination of whether reformist and abolitionist movements have coexisted or treated each other as foreclosing competitors. Theological analysis of whether ethics can be extracted from Dharmasastra without reproducing the legitimacy of the caste framework.',
    'If coexistence is possible, both readings remain live positions and the three-reading kernel remains open. If the readings foreclose each other, the abolitionist reading''s relation to the reformist reading should be coded as ''forecloses'' rather than ''coexists_with''. The framework''s stability depends partly on whether opponents view each other''s readings as incoherent rivals or as different but compatible choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_reading_coexistence_vs_foreclosure, conceptual, 'Logical relationship between reformist and abolitionist readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(dhar_tr_t0, observed).
narrative_ontology:measurement(dhar_tr_t5, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 5, 0.39).
narrative_ontology:measurement_basis(dhar_tr_t5, observed).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(dhar_tr_t10, observed).
narrative_ontology:measurement(dhar_tr_t15, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(dhar_tr_t15, observed).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(dhar_tr_t20, observed).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(dhar_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.85).
narrative_ontology:measurement_basis(dhar_be_t0, observed).
narrative_ontology:measurement(dhar_be_t5, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 5, 0.87).
narrative_ontology:measurement_basis(dhar_be_t5, observed).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 10, 0.88).
narrative_ontology:measurement_basis(dhar_be_t10, observed).
narrative_ontology:measurement(dhar_be_t15, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 15, 0.89).
narrative_ontology:measurement_basis(dhar_be_t15, observed).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 20, 0.89).
narrative_ontology:measurement_basis(dhar_be_t20, observed).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 25, 0.89).
narrative_ontology:measurement_basis(dhar_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.88).
narrative_ontology:measurement_basis(dhar_su_t0, observed).
narrative_ontology:measurement(dhar_su_t5, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 5, 0.89).
narrative_ontology:measurement_basis(dhar_su_t5, observed).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 10, 0.9).
narrative_ontology:measurement_basis(dhar_su_t10, observed).
narrative_ontology:measurement(dhar_su_t15, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 15, 0.91).
narrative_ontology:measurement_basis(dhar_su_t15, observed).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 20, 0.91).
narrative_ontology:measurement_basis(dhar_su_t20, observed).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 25, 0.91).
narrative_ontology:measurement_basis(dhar_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__abolitionist_rejection, 0.02).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, brahmanical_ritual_authority).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, caste_hierarchy_enforcement_mechanisms).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel has three competing readings: abolitionist_rejection, orthodox_literalist, and reformist_contextual. Each reading is a separate constraint story with its own epsilon (extractiveness), beneficiary/victim structure, and classification. They are linked through network.affects_constraints to indicate that changes in one reading (e.g., institutional collapse of abolitionist movements) create pressure on the others. The abolitionist reading specifically forecloses the literalist reading's core claim (eternality of caste) within any single framework that recognizes Dalit and female personhood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
