% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra Orthodox Literalist Interpretation: Eternal Varna/Jati Hierarchy
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The Dharmasastra corpus—especially texts like the Manusmrti—encodes a
 *   four-fold varna hierarchy (brahmins, kshatriyas, vaishyas, shudras) plus
 *   an additional excluded category of dalits (untouchables), with separate
 *   prescriptions for women. The orthodox-literalist reading treats these
 *   prescriptions as eternal, revealed truth (shruti) requiring literal
 *   observance. This reading claims the hierarchy reflects cosmic law (Rta)
 *   and karmic justice: each person's varna and occupation are justified by
 *   past lives and divine order. The extractiveness is extremely high because
 *   the reading concentrates ritual authority, educational access, wealth
 *   accumulation, and political power in the upper castes while prescribing
 *   permanent servitude, occupational confinement, and ritual pollution for
 *   the lower castes and dalits. Suppression is sustained through ritual
 *   authority structures (brahmin gatekeeping of textual interpretation),
 *   family enforcement (guardianship prescriptions for women and children),
 *   and the threat of ritual pollution and social violence. The theater ratio
 *   is moderate: some coordination of occupational specialization is real,
 *   but much enforcement energy defends the hierarchy's permanent status
 *   against reform and challenge.
 *
 * KEY AGENTS:
 *   - Brahmin ritual specialists: agenda-setters who interpret the corpus, control ritual authority, and maintain the literalist reading's institutional power
 *   - Upper castes (kshatriyas, vaishyas): beneficiaries who receive political authority, economic privileges, and social rank from the hierarchy
 *   - Shudras and dalits: expansive victim set bearing enforced occupational confinement, labor extraction, ritual exclusion, and systematic exclusion from education and authority
 *   - Women across all castes: victims subject to guardianship prescriptions and exclusion from ritual initiation and scriptural study
 *   - Reformist scholars and anti-caste movements: excluded from orthodox-literalist institutions; their voices challenge the eternal-truth framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.82).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.79).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.82).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra Orthodox Literalist Interpretation: Eternal Varna/Jati Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '53d7ac77-3f21-4fdc-a63e-3290224296b7').
narrative_ontology:cs_kernel_codification('53d7ac77-3f21-4fdc-a63e-3290224296b7', fixed_text).
narrative_ontology:cs_authority_grounding('53d7ac77-3f21-4fdc-a63e-3290224296b7', lineage).
narrative_ontology:cs_interpretation_layer_present('53d7ac77-3f21-4fdc-a63e-3290224296b7').
narrative_ontology:cs_reading_relation('53d7ac77-3f21-4fdc-a63e-3290224296b7', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_reading_relation('53d7ac77-3f21-4fdc-a63e-3290224296b7', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('53d7ac77-3f21-4fdc-a63e-3290224296b7', foundational, varna_hierarchy_eternally_revealed).
narrative_ontology:cs_axiom_status(varna_hierarchy_eternally_revealed, holdable).
narrative_ontology:cs_axiom_grounding('53d7ac77-3f21-4fdc-a63e-3290224296b7', varna_hierarchy_eternally_revealed, theological).
narrative_ontology:cs_axiom('53d7ac77-3f21-4fdc-a63e-3290224296b7', foundational, caste_prescriptions_require_literal_observance).
narrative_ontology:cs_axiom_status(caste_prescriptions_require_literal_observance, holdable).
narrative_ontology:cs_axiom_grounding('53d7ac77-3f21-4fdc-a63e-3290224296b7', caste_prescriptions_require_literal_observance, deontological).
narrative_ontology:cs_reference_frame('53d7ac77-3f21-4fdc-a63e-3290224296b7', eternally_ordained_cosmic_hierarchy).
narrative_ontology:cs_drift_state('53d7ac77-3f21-4fdc-a63e-3290224296b7', contemporary_post_colonial_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('53d7ac77-3f21-4fdc-a63e-3290224296b7', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_specialists).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_warriors).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaishya_merchants).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits_untouchables).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras_service_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_excluded_ritual).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, tribal_populations).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, eternal_vedic_authority).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, cosmic_order_maintenance).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, karmic_justification_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim exclusive ritual authority grounded in Vedic knowledge. Interpret and transmit the Dharmasastra corpus as revealed truth requiring literal observance. Control admission to scriptural study, ritual performance rights, and authoritative interpretation. Collect ritual fees, maintain educational monopoly, and derive social authority from varna-based rank hierarchy that places them at the apex.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_specialists, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_specialists, beneficiary).

% Occupy the second varna with prescriptive authority to rule, command armies, and extract tribute. The Dharmasastra literalist reading legitimizes their rule as divinely ordained; their power rests on the hierarchy's permanent, revealed status. Social rank, ritual privileges, and ruling authority are framed as eternal and justified by past karma.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_warriors, beneficiary,
    powerful, civilizational, identity_locked, continental).

% The third varna, entitled to conduct trade and accumulate wealth within prescribed bounds. The literalist reading secures their commercial status while subordinating them to brahmin and kshatriya authority. Economic activity is framed as legitimate only within the revealed hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaishya_merchants, beneficiary,
    moderate, civilizational, identity_locked, continental).

% Prescribed to serve the three higher varnas; forbidden from wealth accumulation, ritual study, or independent authority. The Dharmasastra literalist reading explicitly mandates their subordination as eternal law. Denied access to education, ritual participation, and political authority. Subject to the economic extraction and social discipline imposed by the upper varnas.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras_service_castes, payer,
    powerless, civilizational, trapped, continental).

% Placed outside and below the varna system entirely by Dharmasastra literalist prescription. Subjected to extreme ritual pollution restrictions, occupational confinement to degrading work (leather handling, scavenging), and systematic exclusion from temples, public spaces, and all ritual participation. The literalist reading frames this exclusion as eternal and divinely mandated. Face the highest extraction through enforced labor, social violence, and complete denial of status.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits_untouchables, payer,
    powerless, civilizational, trapped, continental).

% Excluded from Vedic ritual initiation, scriptural study, and independent religious authority across all varnas. The Dharmasastra literalist reading prescribes their guardianship by father, husband, and son; denies them property rights and autonomous decision-making. Confined to domestic sphere and reproductive roles. Subject to enforcement via family authority structures and social sanctions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_excluded_ritual, payer,
    powerless, civilizational, identity_locked, continental).

% Classified as mlechchha (barbarian/outsider) by Dharmasastra literalist prescription. Denied full membership in the varna system; subject to conversion pressures, land dispossession, and cultural suppression as non-Vedic peoples. The literalist reading frames their status as outside the cosmic order, justifying their marginalization and economic/cultural extraction.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, tribal_populations, payer,
    powerless, civilizational, trapped, continental).

% Hindu scholars and leaders (19th century onward) who argue the Dharmasastra's ethical core is separable from time-bound caste prescriptions. Excluded from orthodox-literalist institutions and narrative authority; their voices challenge the eternal-truth framing but face resistance from entrenched orthodox authorities.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_scholars, excluded,
    organized, biographical, constrained, continental).

% Activist movements, Dalit scholars, and egalitarian interpreters who reject the Dharmasastra framework entirely as fundamentally oppressive. Structurally barred from orthodox institutions; their advocacy for abolition is treated as heresy or illegitimate by literalist authorities.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, anti_caste_abolitionists, excluded,
    organized, biographical, constrained, continental).

% British administrators documented and selectively codified Dharmasastra prescriptions into colonial law, both reinforcing and disrupting traditional authority structures. Their role as external observers with enforcement power altered the constraint's operation without fully determining its internal legitimacy claims.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, colonial_administrators, observer,
    institutional, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_ritual_specialists).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The literalist reading coordinates a hierarchical social order allegedly grounded in eternal cosmic law (Rta). It claims to solve the problem of social organization by mapping each person's role, obligations, and status into a permanent, divinely-ordained framework. The reading coordinates ritual purity, occupational specialization, and hereditary role assignment into a stable cosmic architecture.
% TRANSFER_FUNCTION: Moves labor (shudra and dalit service, agricultural produce, manual work), ritual authority (concentrated in brahmins), political power (concentrated in kshatriyas), and economic accumulation (favoring vaishyas) from the lower varnas and excluded populations upward to the upper three varnas and brahmin specialists. The reading justifies this extraction as payment for the dharmic order itself.
% ABSENT_VOICES: Dalits, most shudras, women, and tribal populations are systematically excluded from the corpus itself — they have no voice in textual interpretation, no authority to challenge the reading, and no seat in orthodox institutions. The literalist reading treats their exclusion as cosmically justified. Reformist scholars and anti-caste abolitionists are excluded from orthodox-literalist interpretive authority despite occupying real historical and contemporary positions.
% DISAPPEARANCE_RATIONALE: Literalist defenders argue that if the Dharmasastra hierarchy disappeared, cosmic order would collapse and society would fragment into chaos (alluding to the doctrine of kaliyuga decline). Critics argue the world would rearrange into more egalitarian structures, pointing to centuries of anti-caste reform and the partial dissolution of caste enforcement under colonial and post-independence law. The contest is not whether arrangements depend on the constraint, but whether the constraint represents eternal necessity or constructed hierarchy.
% FOUNDING_PROBLEM: The Dharmasastra corpus emerged in a context of early complex agrarian societies (c. 1200–400 BCE) requiring coordination of ritual, occupational specialization, resource distribution, and social authority. The literalist reading claims the solution was revealed as eternal Vedic law: a permanent, cosmic hierarchy that naturally assigns role and status.
% FOUNDING_PROBLEM_CORROBORATION: Hindu reformists (Rammohan Roy, Keshab Chandra Sen onward), Dalit scholars (Ambedkar, Periyar, contemporary Dalit intellectuals), colonial administrators' own records, and comparative social anthropology all attest that the specific coordination problems Dharmasastra addressed (ritual purity in small agrarian kingdoms, occupational specialization) are solved by other mechanisms (democratic governance, occupational mobility, merit-based education). The founding problem is not live; the constraint persists as ideology and institutional practice. The literalist authorities themselves attest differently, maintaining that cosmic order requires the hierarchy's preservation — but this is internal to the reading, not corroboration from outside.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, contested).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness (0.82) reflects the extreme asymmetry: the reading prescribes the upper castes to rule, study, accumulate wealth, and perform all valued rituals, while mandating the lower castes to serve, avoid ritually-sensitive work, and accept permanent subordination. The prescriptions are explicit and detailed across social roles, occupational boundaries, and ritual access. Suppression (0.79) is high because the literalist reading claims the hierarchy is divinely ordained and eternal—challenging it is framed as cosmic transgression. Enforcement runs through brahmin authority (textual interpretation monopoly), family structures (guardianship of women and children), and ritual pollution concepts (keeping dalits at distance and confining them to defiling work). Theater (0.41) is moderate because the coordination story is partly real—occupational specialization, ritual purity disciplines, and social hierarchy do coordinate large agrarian societies—but much enforcement activity explicitly defends the permanent, divinely-ordained status of the hierarchy against the lived pressures of labor mobility, inter-caste contact, and reform movements. The measurement series show high stability across the interval: extractiveness and suppression plateau early (by period 8-12) and remain constant, indicating an entrenched hierarchy maintained by institutional inertia rather than active contestation. This stability is consistent with a tangled-rope reading: the constraint coordinates occupation and ritual, but the coordination is inseparable from asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (brahmin ritual specialists and upper castes) and the victim seats (dalits, shudras, women) compute radically different constraint types from the same structural data. From the brahmin and upper-caste seat, the arrangement is real coordination justified by cosmic order and ethical duty (dharma). From the dalit and shudra seats, the same reading is pure extraction enforced by ritual authority and social violence, with the coordination story as cover. The engine derives directionality from beneficiary/victim declarations and exit options: the beneficiaries have arbitrage-grade exit (can reinterpret or abandon the literalist reading while remaining brahmins); the victims are identity-locked (trapped within the caste hierarchy by birth, with no exit except wholesale rejection of the reading itself or conversion). This exit asymmetry drives the directionality divergence and accounts for the per-seat type divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin ritual specialists: d ≈ 0.1 (full beneficiary). They set the reading, control interpretation, collect ritual fees, and maintain exclusive access to scriptural authority. Their exit is arbitrage-grade: they can adopt reformist readings while preserving brahmin identity, but the literalist reading maximizes their institutional power. Upper castes (kshatriyas, vaishyas): d ≈ 0.25–0.35 (beneficiary-leaning). They receive political authority and economic privileges from the hierarchy, but their exit is constrained by identity-lock: rejecting the literalist reading risks losing their varna status. Shudras: d ≈ 0.75 (target-leaning). They are constrained to service occupations and denied wealth accumulation; their exit is trapped (rejecting the reading does not free them from the occupational and ritual restrictions, which are enforced by broader society). Dalits: d ≈ 0.95 (full target). They are placed outside the system entirely, subjected to ritual pollution and extreme occupational confinement; their exit is trapped and identity-locked (the literalist reading itself defines their exclusion). Women across castes: d ≈ 0.70–0.80 (target, with variation by caste). They are identity-locked by gender and confined to guardianship and domestic roles; their exit requires rejecting the entire framework, which is enforced through family structures and social sanction. The beneficiaries cluster at low d (0.1–0.35); the victim populations cluster at high d (0.70–0.95). This structural asymmetry is the source of the tangled-rope classification: there is real coordination (occupational specialization, ritual order) bundled with extreme asymmetric extraction (concentrated authority, denied exits, prescribed subordination).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—coordination of occupational roles and ritual purity in early agrarian societies—is documented as DEAD by reformist scholars, Dalit historians, and colonial administrative records. The literalist reading persists through institutional inertia (brahmin gatekeeping of scriptural interpretation, family enforcement of prescriptions) and the bundling of coordination function with extraction. The constraint does not persist because participants believe the founding problem is live; it persists because the beneficiary seats (brahmins and upper castes) extract substantial authority and status from the literalist reading, and because the victim seats (dalits, shudras, women) lack the institutional power to overturn the reading without external support. This is a classic mandatrophy signature: the founding mandate (cosmic coordination) is obsolete (occupational mobility, inter-caste contact, democratic governance solve the problem differently), but the extraction mechanism (ritual authority, occupational confinement, exclusion from education) persists because it benefits concentrated seats. The theater ratio (0.41) is not high enough to classify as piton—the reading still coordinates some occupational roles—but the persistence-despite-dead-founding-problem is the diagnostic signal for mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression primarily structural (brahmin authority barriers, legal/economic enforcement of occupational confinement) or internalized (dalits and shudras accepting the literalist reading''s karmic justifications as legitimate)?',
    'Ethnographic and historical evidence of resistance, exit attempts, and conversion patterns. Dalit autobiography and movement testimony. Post-exit behavioral data: if suppression persists after the literalist reading''s institutional authority erodes (e.g., in secular contexts or outside brahmin institutions), the suppression is partly internalized.',
    'If structural, the constraint''s effective suppression could be reduced by dismantling brahmin gatekeeping and enforcement institutions. If internalized, suppression persists even after structural barriers fall, requiring cognitive/ideological decolonization alongside institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression is maintained through institutional barriers or through internalized acceptance of the literalist reading''s legitimacy').

omega_variable(
    eternal_law_vs_constructed_hierarchy,
    'Is the Dharmasastra literalist reading a genuine codification of eternal cosmic law (as the reading claims) or a retrospective naturalization of constructed social hierarchies that benefited particular seats?',
    'Comparative historical analysis: the Dharmasastra texts show variation, revision, and adaptation across time and region—evidence that the prescriptions are authored documents reflecting specific historical contexts, not eternal revelations. Textual criticism establishing the composition dates and authorial contexts of Manusmrti and other texts. Analysis of alternative Vedic and non-Vedic traditions (e.g., Jain and Buddhist ethical frameworks) that offer different solutions to coordination problems without caste hierarchy.',
    'If the reading is a constructed naturalization, it loses the claim to eternity and cosmic necessity; the hierarchy becomes a historical arrangement open to reform or abandonment. If eternal, the reading''s resistance to change is justified by cosmic order; reform is framed as transgression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eternal_law_vs_constructed_hierarchy, empirical, 'Whether the Dharmasastra prescriptions reflect eternal cosmic law or historically-constructed social hierarchies treated as natural').

omega_variable(
    coordination_extraction_inseparability,
    'Is the occupational specialization and ritual coordination the literalist reading provides structurally inseparable from the caste hierarchy''s asymmetric extraction, or could the coordination functions be preserved without the permanent, prescribed subordination of lower castes?',
    'Comparative institutional analysis of occupational specialization in non-caste societies (e.g., medieval Europe, Islamic societies, contemporary secular economies). Historical evidence of occupational mobility and inter-caste contact in regions where literalist brahmin authority weakened (e.g., colonial and post-independence India). Thought experiment: what coordination would break down if dalits were admitted to ritual performance and scriptural study?',
    'If inseparable, the extraction is the price of the coordination; if separable, the extraction is pure rent-seeking riding on a real but modest coordination function. This determines whether the constraint is structurally tangled_rope (coordination + extraction bundled) or snare (extraction with a vestigial cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, conceptual, 'Whether the occupational coordination function requires the permanent caste hierarchy or could be preserved through other mechanisms').

omega_variable(
    mandatrophy_corroboration,
    'Is the founding problem (coordination of occupational roles and ritual purity in agrarian kingdoms c. 1200–400 BCE) genuinely solved by alternative mechanisms in contemporary contexts, or does it remain live in some regions or communities?',
    'Historical and ethnographic data from contexts where the literalist Dharmasastra authority has weakened (colonial India, post-independence secular governance, urban centers): do occupational roles and ritual purity show different coordination mechanisms, or do they collapse? Testimony from communities that have abandoned literalist prescription but retained some occupational and ritual structure. Analysis of whether democracy, merit-based education, and labor markets provide equivalent coordination without the varna hierarchy.',
    'If the founding problem is solved by alternatives, the mandatrophy classification is supported: the reading persists despite dead founding mandate. If the founding problem remains live, the reading retains some coordination function and should not be classified as mandatrophic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_corroboration, empirical, 'Whether the founding coordination problem is solved by alternative mechanisms in contemporary India or remains live in significant populations').

omega_variable(
    reading_foreclosure_coexistence_boundary,
    'Does the orthodox-literalist reading''s core claim (eternal, revealed varna prescriptions) logically foreclose the reformist-contextual reading (ethical dharma separable from time-bound caste), or can both readings coexist as live positions held by different institutional seats?',
    'Analysis of whether a single authority structure (e.g., a Hindu denomination, a scholarly tradition) can coherently hold both readings without incoherence. Evidence from actual institutional attempts at synthesis or pluralism.',
    'If foreclosure is discovered (one reading''s core premise directly contradicts the other''s), the engine will compute foreclosure in the cs_structure. If coexistence is confirmed, the readings remain live alternatives held by different seats. This affects how challenges to the literalist reading propagate through the kernel and across sibling constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_coexistence_boundary, conceptual, 'Whether the literalist reading logically forecloses the reformist reading or both remain live positions in institutional competition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(dhar_tr_t0, observed).
narrative_ontology:measurement(dhar_tr_t4, dharmasastra_corpus__orthodox_literalist, theater_ratio, 4, 0.39).
narrative_ontology:measurement_basis(dhar_tr_t4, observed).
narrative_ontology:measurement(dhar_tr_t8, dharmasastra_corpus__orthodox_literalist, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(dhar_tr_t8, observed).
narrative_ontology:measurement(dhar_tr_t12, dharmasastra_corpus__orthodox_literalist, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(dhar_tr_t12, observed).
narrative_ontology:measurement(dhar_tr_t16, dharmasastra_corpus__orthodox_literalist, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(dhar_tr_t16, observed).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__orthodox_literalist, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(dhar_tr_t20, observed).
narrative_ontology:measurement(dhar_tr_t24, dharmasastra_corpus__orthodox_literalist, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(dhar_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(dhar_be_t0, observed).
narrative_ontology:measurement(dhar_be_t4, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 4, 0.79).
narrative_ontology:measurement_basis(dhar_be_t4, observed).
narrative_ontology:measurement(dhar_be_t8, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 8, 0.81).
narrative_ontology:measurement_basis(dhar_be_t8, observed).
narrative_ontology:measurement(dhar_be_t12, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 12, 0.82).
narrative_ontology:measurement_basis(dhar_be_t12, observed).
narrative_ontology:measurement(dhar_be_t16, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 16, 0.82).
narrative_ontology:measurement_basis(dhar_be_t16, observed).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 20, 0.82).
narrative_ontology:measurement_basis(dhar_be_t20, observed).
narrative_ontology:measurement(dhar_be_t24, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(dhar_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(dhar_su_t0, observed).
narrative_ontology:measurement(dhar_su_t4, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 4, 0.76).
narrative_ontology:measurement_basis(dhar_su_t4, observed).
narrative_ontology:measurement(dhar_su_t8, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 8, 0.77).
narrative_ontology:measurement_basis(dhar_su_t8, observed).
narrative_ontology:measurement(dhar_su_t12, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 12, 0.78).
narrative_ontology:measurement_basis(dhar_su_t12, observed).
narrative_ontology:measurement(dhar_su_t16, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 16, 0.79).
narrative_ontology:measurement_basis(dhar_su_t16, observed).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 20, 0.79).
narrative_ontology:measurement_basis(dhar_su_t20, observed).
narrative_ontology:measurement(dhar_su_t24, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 24, 0.79).
narrative_ontology:measurement_basis(dhar_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.18).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading with different ε values and beneficiary/victim structures. The orthodox-literalist reading (this file) treats the varna prescriptions as eternal, generating high extraction (0.82) concentrated in upper castes. The reformist-contextual reading (sibling file) treats the ethical dharma as separable from time-bound prescriptions, reducing extraction to the coordination function alone. The abolitionist-rejection reading (sibling file) treats the entire framework as oppressive, with zero legitimate authority. These are not the same constraint viewed from different angles; they are three different constraints sharing a kernel (the Dharmasastra texts) but differing in ε, victim sets, and legitimacy claims. The literalist reading influences both siblings by establishing the institutional and interpretive authority they must challenge or reframe.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
