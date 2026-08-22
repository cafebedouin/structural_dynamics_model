% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections: Restrictive Reading (Individual Worship Only)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Lausanne Treaty of 1923 established protections for religious
 *   minorities in Turkey following the post-WWI population exchanges and
 *   territorial settlements. Under the restrictive reading, Lausanne
 *   guarantees only individual rights to worship and private religious
 *   practice; institutional matters — the governance structures, property
 *   ownership, theological education, and legal personality of minority
 *   religious organizations — remain subject to Turkish domestic law and
 *   state sovereignty. This reading privileges state control over minority
 *   institutional capacity, allowing the Turkish state to restrict or deny
 *   property restitution, educational authorization, and organizational
 *   self-governance to minority religious bodies. The constraint operates as
 *   a snare: it uses the language of minority protection (Lausanne's nominal
 *   coordination function) as cover for systematic institutional extraction
 *   and exclusion. The measurement series shows extraction accumulating
 *   steadily from 1923 onward (t=0 baseline 0.55 → 2023 endpoint 0.82), with
 *   theater rising as enforcement increasingly relies on performative
 *   invocations of 'Lausanne protections' while real protections narrow. The
 *   reading is one of three structurally distinct interpretations of the same
 *   treaty kernel.
 *
 * KEY AGENTS:
 *   - Turkish state apparatus (agenda_setter, powerful, institutional): interprets and enforces the restrictive reading; benefits from unchallenged control over minority institutional regulation
 *   - Minority religious institutions (snare victims, organized/institutional, trapped exit): denied institutional autonomy, property recognition, and theological education authorization; forced to operate under general Turkish law designed for majority-religion state relations
 *   - Minority clergy and theologians (victims, organized/moderate, identity-locked exit): excluded from formal training pathways, restricted in legal authority to perform sacraments and administer community affairs
 *   - Minority property holders (victims, moderate/powerful, constrained exit): unable to reclaim or maintain properties seized during 1923 exchanges or subject to confiscation under domestic law; property claims routed through legal regimes designed to deny restitution
 *   - Guarantor states and European human rights institutions (observers, institutional/analytical, arbitrage exit): position themselves as external adjudicators of Lausanne's meaning but lack enforcement power if Turkey rejects their interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.82).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.76).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections: Restrictive Reading (Individual Worship Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '3bea9855-e696-48af-a461-48b6025cbb09').
narrative_ontology:cs_kernel_codification('3bea9855-e696-48af-a461-48b6025cbb09', fixed_text).
narrative_ontology:cs_authority_grounding('3bea9855-e696-48af-a461-48b6025cbb09', extraction).
narrative_ontology:cs_interpretation_layer_present('3bea9855-e696-48af-a461-48b6025cbb09').
narrative_ontology:cs_reading_relation('3bea9855-e696-48af-a461-48b6025cbb09', lausanne_minority_protections__expansive_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bea9855-e696-48af-a461-48b6025cbb09', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('3bea9855-e696-48af-a461-48b6025cbb09', foundational, institutional_matters_are_domestic_law_domain).
narrative_ontology:cs_axiom_status(institutional_matters_are_domestic_law_domain, holdable).
narrative_ontology:cs_axiom_grounding('3bea9855-e696-48af-a461-48b6025cbb09', institutional_matters_are_domestic_law_domain, conventional).
narrative_ontology:cs_axiom('3bea9855-e696-48af-a461-48b6025cbb09', foundational, individual_worship_exhausts_lausanne_minority_protection_scope).
narrative_ontology:cs_axiom_status(individual_worship_exhausts_lausanne_minority_protection_scope, holdable).
narrative_ontology:cs_axiom_grounding('3bea9855-e696-48af-a461-48b6025cbb09', individual_worship_exhausts_lausanne_minority_protection_scope, deontological).
narrative_ontology:cs_reference_frame('3bea9855-e696-48af-a461-48b6025cbb09', state_sovereignty_framework).
narrative_ontology:cs_drift_state('3bea9855-e696-48af-a461-48b6025cbb09', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3bea9855-e696-48af-a461-48b6025cbb09', '2026-06-11T09:00:00Z').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_clergy_and_theologians).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_property_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the restrictive reading of Lausanne. Controls the regulatory apparatus (civil law, property registry, educational accreditation) through which institutional protections are denied or granted. Benefits from consolidated control over minority institutional governance, property allocation, and educational authorization. Has the power to grant exceptions (restitution cases, educational permits) on a case-by-case basis, which reinforces its position as gatekeeper. The state apparatus views the restrictive reading as a legitimate expression of national sovereignty and sees the enforcement of domestic-law frameworks as the normal expression of state authority, not as extraction.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Denied institutional autonomy, legal entity status, and property recognition under the restrictive reading. Forced to operate within frameworks designed for majority-religion state relations, which do not accommodate minority institutional structures (diaspora governance, theological education not controlled by state, property held in trust or collectively). Unable to train clergy formally through recognized theological schools; education occurs underground or abroad. Trapped between operational necessity (needing institutional capacity to serve their communities) and legal inability to formalize that capacity. Exit options are severely constrained: leaving Turkey means abandoning the institutional base, and operating within Turkey means accepting subordinate legal status. Identity is fused with institutional continuity — for these communities, institutional autonomy is not a negotiable policy preference but a core element of religious and cultural identity.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_institutions, payer,
    organized, generational, identity_locked, national).

% Excluded from formal theological education authorized by the state; cannot legally practice ministerial functions outside narrowly defined individual-worship contexts. Required to operate under constant legal jeopardy — ordination, pastoral counseling, sacramental administration all exist in legal grey zones. Migration to diaspora communities offers exit, but leaving constitutes abandonment of their institutional roles and pastoral communities. Career trajectories are foreclosed: advancement within an institutional hierarchy is impossible when the hierarchy itself is not recognized. Professional identity is entirely constituted through the religious community, making exit identity-destroying. Clergy represent the institutional capacity the restrictive reading most systematically denies: they are the nodes of institutional continuity and theological authority, precisely what the state apparatus seeks to control.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_clergy_and_theologians, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, minority_clergy_and_theologians, excluded).

% Unable to reclaim, maintain, or transfer properties under the restrictive reading's domestic-law framework. Properties seized during 1923 exchanges remain state-owned; properties subsequently acquired by religious institutions cannot be held in institutional names and are subject to expropriation or reclassification as public land. Restitution claims are routed through legal frameworks explicitly designed to exclude them (e.g., properties must be 'currently used for worship' to gain limited protection, excluding schools, theological seminaries, administrative centers). Exit options exist (property can be sold or abandoned), but doing so surrenders the institution's material base. Mobility is constrained by property dependency: institutions are rooted in physical places, and legal inability to own property in institutional names makes long-term institutional planning impossible.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_property_holders, payer,
    moderate, biographical, constrained, national).

% European Court of Human Rights, UN human rights bodies, and guarantor states (France, UK, Greece, and others under the Lausanne protocol structure) can rule the restrictive reading incompatible with ECHR or international human rights law, but Turkey retains the power to resist or reinterpret those rulings. Observers can apply diplomatic pressure, issue recommendations, or condition trade/EU membership on minority protections, but Turkey's institutional power within its sovereign territory limits the guarantors' enforcement capacity. Turkey has shown willingness to reject or minimize international human rights rulings on minority issues (e.g., ECHR cases on religious freedom are acknowledged but circumvented through domestic administrative workarounds). The guarantor role is fundamentally asymmetric: guarantors can see the snare and name it, but they cannot unilaterally unwind the extraction without Turkey's cooperation or consent.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states_and_european_institutions, observer,
    institutional, generational, arbitrage, global).

% Document violations, bring cases to European courts, advocate for expansive-reading interpretation of Lausanne. Largely excluded from official regulatory processes (Turkey's domestic legal interpretation is made by state apparatus and courts, where advocates lack standing). Their advocacy shifts the theater ratio upward (the state must perform 'minority protection' rhetoric in response to their campaigns), but their exclusion from decision-making authority means the snare persists. International advocacy has shifted some enforcement modality (the state increasingly uses administrative discretion rather than explicit law to deny institutional rights, to evade international scrutiny), but the core extraction mechanism is intact.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, human_rights_advocates_and_ngos, excluded,
    moderate, biographical, mobile, global).

% Provides an analytical seat external to Turkey's jurisdiction. Can declare practices incompatible with ECHR, issue judgments, and recommend remedies, but enforcement depends on Turkey's willingness to comply. Rulings have shifted some institutional practices (e.g., property restitution in some cases, recognition of minority educational institutions in certain contexts), but the restrictive-reading interpretation persists as Turkey's official position. The mechanism represents an alternative to the restrictive reading (the guarantor reading involves European institutional oversight), so it is observer rather than party to this constraint.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_human_rights_mechanisms, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally: protects individual minority worship rights and honors Lausanne's commitment to minority religious freedom. Structurally: establishes a legal framework that allows the state to distinguish between protected individual religious practice and unprotected institutional capacity, creating a category boundary that enables institutional extraction without appearing to violate treaty obligations.
% TRANSFER_FUNCTION: Moves institutional capacity, property ownership, and educational authority from minority religious organizations to the Turkish state apparatus. Minority institutions surrender control over their own governance, property, and clergy training in exchange for the privilege of individual worship (which would exist anyway under Turkish constitutional protections). The transfer is one-way and non-negotiable: minorities cannot bargain for institutional autonomy in exchange for accepting state oversight.
% ABSENT_VOICES: Minority institutional leaders whose voices would challenge the restrictive reading are partially excluded from official legal interpretation (they are heard in courts, but courts apply restrictive-reading doctrine as law). The expansive-reading community (including diaspora minorities and international human rights advocates) is largely excluded from domestic Turkish legal discourse. Guarantor states' voices enter through diplomatic channels and European courts, not through Turkish domestic regulatory processes. The primary absence is the theoretical voice of 'Lausanne's actual drafters' — original intent is archived and not accessible to contemporary interpretation.
% DISAPPEARANCE_RATIONALE: If the restrictive-reading constraint vanished (i.e., if Turkey adopted the expansive reading or guarantor reading), institutional autonomy would be restored, property restitution would commence, theological schools would be formally recognized, and minority institutions would regain legal personality. The organizational landscape would restructure: minority institutions would formalize hierarchies, reclaim historical properties, establish accredited educational systems. The state apparatus would lose gatekeeping power over minority institutional capacity. This is not a marginal regulatory shift — it would represent a fundamental reallocation of institutional authority. The world would rearrange around the restored institutional capacity.
% FOUNDING_PROBLEM: After the 1923 population exchanges and territorial settlement, Turkey faced a problem of defining state sovereignty boundaries: should minority religious institutions retain autonomous governance and property rights from the Ottoman era, or should all religious organization fall under state supervision via Turkish domestic law? The restrictive reading resolves this by subordinating institutional matters to state law, preserving individual worship rights (Turkey's international obligation) while consolidating state control over institutional organization.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state apparatus attests the founding problem is still live: Turkey must maintain state sovereignty over institutional regulation to prevent fragmentation and ensure legal uniformity. Human rights advocates and guarantor states attest the founding problem was solved decades ago and the restriction now serves no coordination purpose — it is purely extractive. European human rights courts have ruled (Şahin v. Turkey, 2005; Metropolitan Church of Bessarabia v. Moldova, 2001) that the founding problem of state sovereignty does not justify denying institutional religious autonomy. Independent scholars of Lausanne history and international law (including non-Turkish experts) generally support the guarantor/expansive reading: the treaty's drafters intended institutional protections, not their exclusion. The state apparatus is the only major corroborating source for the restrictive reading; external voices from law, human rights, and Treaty scholarship support alternative readings.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 terminal) because the restrictive reading allows systematic institutional foreclosure without breaking the nominal treaty commitment: minorities retain individual worship rights (the constraint's coordination face) while institutional capacity — property, education, governance, legal personality — is extracted via domestic-law subordination (the extraction face). Suppression is high (0.76) because the enforcement machinery requires active denial of property claims, educational accreditation, and legal entity status; these are not passive defaults but require state apparatus action. Theater is substantial and rising (0.41 terminal, 0.12 origin) because enforcement increasingly relies on ritual invocation of 'Lausanne protections for minorities' while systematically narrowing what those protections mean — performative minority-protection rhetoric masks institutional extraction. Accessibility collapse (0.68) reflects that minorities' alternatives — seeking institutional autonomy through European human rights law, UN mechanisms, or private-law workarounds — are themselves constrained by the state's gatekeeping over property access and legal entity status. Resistance (0.59) indicates organized minority opposition (legal challenges, advocacy campaigns, institutional adaptation strategies) without yet achieving structural reversal; the resistance is real but the institutional extraction persists. The measurement trajectory shows monotonic extraction accumulation: this is not oscillation or cyclical enforcement but steady ratcheting of the restriction's scope and severity as case law and administrative practice narrow institutional protections further.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state apparatus and the restrictive-reading interpretive community see a legitimate, treaty-compliant allocation: minorities get individual worship (the core protection), and institutional matters stay in domestic sovereignty (the rational limit). From this seat, the constraint is coordination (a way to honor Lausanne while preserving state control). From the minority institutions' seats, the same structure operates as a snare: the coordination rhetoric ('individual worship is protected') conceals the extraction ('but your institution, property, and education are not'). The engine will compute different types for these seats: the state apparatus seat may perceive coordination or tangled_rope (enforced but justified); the victim seats will compute snare (coercive, no exit, asymmetric). This divergence is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the clear beneficiary (d → 0.1–0.2): it collects institutional control, property assets, educational gatekeeping, and organizational hierarchy. Minority institutions and clergy are the targets (d → 0.85–0.95): they bear the extraction directly through institutional denial, property loss, and legal subordination. Guarantor states are observers (d → 0.5, analytical seat): they can see the structure but lack enforcement power within Turkey's jurisdiction. The directionality is stable across power levels: whether the minority institution is a small ethnic church (moderate power, trapped/identity_locked exit) or a larger denomination (powerful, constrained exit), the directional relationship — they are extracted from — does not shift. The extraction only scales with organizational capacity: more powerful institutions face fiercer institutional denial; weaker institutions are often simply ignored until they try to assert property or educational claims, at which point suppression activates.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive reading shows no mandatrophy — it continues to serve a living mandate for the Turkish state (institutional control consolidation, property sovereignty, educational gatekeeping). The founding problem was real (post-1923 territorial settlement, population exchange, need to define state sovereignty boundaries), and the state's mandate to interpret it narrowly remains live. However, the constraint exhibits symptoms of mandatrophy at the minority institutional level: the minorities' founding mandate was institutional continuity and self-governance (the expansive reading's claim), and that mandate is dead — minorities have no realistic path to institutional autonomy under the restrictive reading. The mandatrophy is asymmetric: the state's mandate persists; the victim-community's founding mandate is extinct. This is the definition of a snare: the state has the mandate to maintain control, and the maintenance mechanism (the restrictive reading) persists indefinitely because it serves the state, not because it solves a coordination problem that everyone wants solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contract_interpretation,
    'Does the Lausanne Treaty''s minority protections clause contain an implicit guarantee of institutional continuity and self-governance, or does it genuinely restrict protection to individual worship rights only?',
    'Comparative treaty-law analysis: examination of original negotiation records (1923), contemporaneous state practice, and established canons of contract interpretation (including the Vienna Convention on Law of Treaties, Article 31–32). Expert testimony from international law scholars outside the restricting state''s government.',
    'If the treaty contract is read to guarantee institutional autonomy and property rights, this reading''s entire extraction claim collapses — the snare classification reverses to tangled_rope (asymmetric extraction defended as coordination) or mountain (extraction is illusory, the treaty is what it says). The foundational axiom status flips from holdable to overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contract_interpretation, empirical, 'Whether the treaty text itself constrains this reading''s interpretation.').

omega_variable(
    european_human_rights_law_constraint,
    'Do European human rights law doctrines (ECHR Art. 9 on freedom of conscience and religion, Article 11 on freedom of association, Protocol 1 Art. 1 on property) functionally supersede the restrictive reading''s claim that institutional matters are purely domestic?',
    'European Court of Human Rights case law on minority religious institutions (Şahin v. Turkey, Eur. Court H.R. (2005); Metropolitan Church of Bessarabia v. Moldova, Eur. Court H.R. (2001); Aghdgomelashvili v. Georgia, Eur. Court H.R. (2023)). Jurisdictional analysis of whether Turkey''s commitments to the ECHR override domestic-law interpretation of Lausanne.',
    'If European human rights law constrains the restrictive reading, institutional protections re-enter the constraint''s scope via a parallel enforcement regime (guarantor state mechanism). This does not foreclose the restrictive reading structurally, but it reduces the state apparatus''s practical capacity to enforce the domestic-law claim unilaterally. The piton-like theater ratio rises as enforcement must maintain its reading against concurrent legal challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(european_human_rights_law_constraint, empirical, 'Whether supranational human rights mechanisms functionally constrain the restrictive reading''s legal claim.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.76) primarily structural (legal barriers, institutional exclusion, resource denial) or has it become substantially internalized in minority communities'' self-understanding of what they can claim?',
    'Post-exit suppression trajectory: if minority institutions that successfully exit the jurisdiction or achieve property restitution stop carrying suppressive self-limitation, reclassify as primarily structural. Long-form interviews with minority clergy, institutional leadership, and younger-generation members on whether constraints are understood as legal limit or as internal norm.',
    'If largely internalized, the effective suppression is higher than the structural measure — the target carries the constraint with them after legal change. The classification holds snare. If primarily structural, remedy via legal change is faster, and the constraint''s persistence becomes more dependent on active state enforcement (suppression_requirement rises relative to suppression).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in minority institutional identity.').

omega_variable(
    reading_sibling_foreclosure_structure,
    'Can the restrictive reading and the expansive reading coexist within a single coherent treaty framework, or does adoption of one reading''s core premise logically foreclose the other?',
    'Formal contract-interpretation analysis: are the readings incompatible axioms about the same contractual clause, or are they compatible readings that differ on scope/weight? If incompatible, which reading the international consensus (via European Court, UN human rights bodies, comparative state practice) has endorsed. If compatible, document the structural conditions that allow them to coexist (e.g., different jurisdictional domains, time-indexed differences).',
    'If the readings foreclose each other (true incompatibility), reclassify the network relationship to forecloses (the restrictive reading structurally rules out the expansive reading in a single framework, not coexisting). If they can coexist, the coexists_with relationship holds but the omega documents why they appear incompatible to each reading''s adoptees.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure_structure, conceptual, 'Structural relationship between restrictive and expansive readings: foreclosure vs. coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(laus_tr_t5, lausanne_minority_protections__restrictive_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(laus_tr_t10, lausanne_minority_protections__restrictive_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(laus_tr_t15, lausanne_minority_protections__restrictive_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__restrictive_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(laus_tr_t25, lausanne_minority_protections__restrictive_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(laus_be_t5, lausanne_minority_protections__restrictive_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(laus_be_t10, lausanne_minority_protections__restrictive_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(laus_be_t15, lausanne_minority_protections__restrictive_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__restrictive_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(laus_be_t25, lausanne_minority_protections__restrictive_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(laus_su_t5, lausanne_minority_protections__restrictive_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(laus_su_t10, lausanne_minority_protections__restrictive_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(laus_su_t15, lausanne_minority_protections__restrictive_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__restrictive_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(laus_su_t25, lausanne_minority_protections__restrictive_reading, suppression_requirement, 25, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__restrictive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel decomposes into three constraint stories, each instantiating a different reading with different ε values and beneficiary structures. The restrictive reading (this story) characterizes the arrangement as a snare: minorities receive nominal individual-worship protection while institutional capacity is extracted. The expansive reading characterizes the same treaty text as guaranteeing institutional continuity (rope or mountain). The guarantor reading characterizes Lausanne as internationally enforceable (tangled rope with external enforcement). These are not observables of the same constraint — they are different constraints produced by different treaty interpretations. Each story carries its own ε, its own stakeholders, and its own six-questions answers. All three are linked via network.affects_constraints to document that they are readings of one kernel, not independent claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
