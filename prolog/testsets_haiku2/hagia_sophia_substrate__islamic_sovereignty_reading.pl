% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty Reading: 1453 Conquest and Waqf Legitimacy
 *   domain: cultural_heritage/religious_authority/sovereignty
 *
 * SUMMARY:
 *   The Hagia Sophia, constructed as a Byzantine Christian cathedral in 537
 *   CE, was conquered by the Ottoman Empire in 1453 and converted to a
 *   mosque. Under the Ottoman waqf system, it was endowed as Islamic
 *   religious property — a transformation that established Islamic legitimacy
 *   through continuous unbroken practice and formal endowment. In 1934,
 *   Atatürk's secular nationalist government converted it to a museum, a
 *   radical reversal that placed the site under UNESCO-style cosmopolitan
 *   heritage administration and closed it as functioning mosque. In 2020,
 *   under the AKP government, an executive decree reversed the museum status
 *   and restored it to mosque function, reasserting the Islamic sovereignty
 *   reading: the site's legitimacy derives from the 1453 conquest and
 *   continuous waqf endowment, making it sovereign Islamic worship space
 *   under Turkish state authority. This constraint story author instantiates
 *   ONLY the Islamic sovereignty reading — the kernel (Hagia Sophia itself)
 *   is contested by three structural-distinct readings (Islamic sovereignty,
 *   Orthodox restitution, universal heritage), but this story does not
 *   describe all three. This story captures the reading that treats the 1453
 *   conquest and waqf system as the authoritative legitimacy baseline.
 *
 * KEY AGENTS:
 *   - AKP political coalition: institutional beneficiary and agenda-setter; enforces the constraint through state decree and court reversal; collects political capital and electoral support from Islamic constituencies.
 *   - Turkish Islamic constituency: organized beneficiary; gains symbolic restitution and religious representation; stake depends on continued AKP electoral support.
 *   - Non-Muslim visitors: powerless victims; bear access restrictions and transformed experience; exit options limited to other sites.
 *   - UNESCO heritage regime: institutional excluded; jurisdiction denied by Turkish sovereignty claim; enforcement capacity limited to objection.
 *   - Secular Turkish citizens: moderate-power victims; experience ideological defeat and loss of secular inheritance; politically feasible but costly exit options.
 *   - Orthodox ecclesiastical authorities: institutional excluded; cut out of restitution or joint-authority negotiation by sovereignty claim.
 *   - Turkish state apparatus: institutional agenda-setter; administers the decree through courts, appointments, and enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Reading: 1453 Conquest and Waqf Legitimacy").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/religious_authority/sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '8f565bf5-e4fe-4f49-9f16-65c05ae3cc75').
narrative_ontology:cs_kernel_codification('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', fixed_text).
narrative_ontology:cs_authority_grounding('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', extraction).
narrative_ontology:cs_interpretation_layer_present('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75').
narrative_ontology:cs_reading_relation('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', foundational, conquest_derived_sovereignty_supreme).
narrative_ontology:cs_axiom_status(conquest_derived_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', conquest_derived_sovereignty_supreme, conventional).
narrative_ontology:cs_axiom('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', foundational, ottoman_waqf_lineage_immutable).
narrative_ontology:cs_axiom_status(ottoman_waqf_lineage_immutable, holdable).
narrative_ontology:cs_axiom_grounding('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', ottoman_waqf_lineage_immutable, deontological).
narrative_ontology:cs_axiom('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', secondary, secular_museum_appropriation_illegitimate).
narrative_ontology:cs_axiom_status(secular_museum_appropriation_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', secular_museum_appropriation_illegitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', ottoman_islamic_authority).
narrative_ontology:cs_drift_state('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', contemporary_2026, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8f565bf5-e4fe-4f49-9f16-65c05ae3cc75', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secular_turkish_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, international_conservative_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the constraint through 2020 executive decree converting the site from museum to mosque. Enforces through control of state apparatus, court validation (reversing 1934 precedent), and appointments (imam, waqf administration). Collects electoral support from Islamic constituencies and geopolitical legitimacy among Sunni powers. The constraint's persistence depends on AKP electoral dominance; electoral defeat would enable reversal.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains symbolic restitution and religious representation after 86 years of secular museum administration. Experiences the constraint as moral correction and dignity restoration. Their exit is political: supporting opposition parties that would reverse the decree, but this carries high cost in an increasingly Islamizing political environment. Stake directly depends on AKP's continued electoral support.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% Non-agent entity representing broader Islamic world. Receives symbolic restitution of a major Islamic site under Turkish Sunni stewardship, framed as recovery of dignity after Ottoman-to-secular rupture. The ummah does not enforce the constraint; Turkish state power alone maintains it. Symbolic participation is rhetorical and narrative.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, beneficiary,
    powerful, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, observer).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).

% Face structural access restrictions (prayer times closed to non-Muslims, dress codes, photography bans, limited interior access). The constraint makes the site function as active mosque, which structurally excludes casual visitation. Their exit is available (visit other sites) but the Hagia Sophia's unique historical and architectural status makes alternatives imperfect substitutes. They bear the cost of transformed experience and reduced access without participating in the beneficiary structure.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, biographical, constrained, global).

% Structurally barred from adjudicating the site's status by Turkish assertion of state sovereignty over religious property. UNESCO designation becomes advisory; heritage regime's jurisdiction is terminated by the decree and its court validation. The regime can object (World Heritage Committee delisting) but cannot enforce alternative framings within Turkish jurisdiction. Excluded not through negotiation but through authority denial.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime, excluded,
    institutional, generational, trapped, global).

% Experience the constraint as ideological defeat and loss of secular inheritance. The 1934 museum ruling was foundational to Turkish secular modernization (Atatürk's secularism); the 2020 decree reverses that victory. They retain some political voice (opposition parties, civil society) and exit is theoretically feasible (migrate, vote opposition), but both are costly in an increasingly Islamizing political environment. They bear the cost of the reversed legitimacy narrative and lost claim to the site as shared secular heritage.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secular_turkish_citizens, payer,
    moderate, biographical, constrained, national).

% Excluded from contention over the site's status under the Islamic sovereignty reading, which forecloses the Orthodox restitution claim by establishing Turkish Islamic authority as legitimate baseline. The Ecumenical Patriarchate and other Orthodox bodies could advocate for restitution or interfaith administration, but the constraint's enforceability rests on Turkish state monopoly. Orthodox institutions have no enforcement capacity within Turkish jurisdiction; their exclusion is structural to the sovereignty claim.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_ecclesiastical_authority, excluded,
    institutional, generational, trapped, global).

% Administers and enforces the constraint through courts (reversing 1934 precedent), appointments (imam, waqf management, security), and police enforcement (access control, prayer time enforcement). The state is nominally religiously neutral under the 1982 Constitution, but the decree and court validation establish state power in service of Islamic authority. Constraint persistence depends on state's continued exercise of enforcement capacity; change in ruling coalition would enable reversion.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain geopolitical advantage from Turkey's assertion of Islamic state authority and challenge to Western-led universal heritage regime. Conservative state actors (Middle East, Central Asia) receive signal that Turkey is credible defender of Islamic interests and state sovereignty against universalism. Primarily symbolic benefit; enforcement costs borne by Turkey.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_conservative_actors, beneficiary,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates Turkish religious, national, and political identity around a unified legitimacy frame: the conquest-derived Islamic authority provides coherent narrative binding Ottoman continuity, Turkish Sunni leadership, and state power into a single institutional arrangement. The constraint coordinates disparate constituencies (Islamic voters, nationalist elements, conservative regional powers, Turkish state apparatus) around a shared claim to the site's legitimacy.
% TRANSFER_FUNCTION: Transfers symbolic capital, political legitimacy, and access authority from cosmopolitan/secular/neutral framing (museum reading) to Islamic/Turkish/nationalist framing (mosque reading). What moves: authority over interpretive legitimacy shifts from UNESCO-style universal heritage to Turkish state-Islamic authority. Non-Muslim visitors transfer reduced access and transformed experience. Secular Turks transfer claim to secular inheritance. Orthodox Christianity transfers any hope of restitution or joint authority.
% ABSENT_VOICES: Orthodox ecclesiastical authorities (excluded by sovereignty claim), UNESCO and international heritage advocates (jurisdiction denied), Turkish secularists and intellectuals (represented institutionally but marginalized in enforcement), international human-rights bodies (advisory only), indigenous Islamic minority voices seeking alternatives to state Islam (unrepresented). These constituencies would challenge the core premise — that state decree can unilaterally convert shared heritage to religious exclusivity — but are structurally absent from the enforcement apparatus.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared and the site reverted to museum status, the political consolidation achieved by the AKP would dissolve immediately; Islamic constituency support would fracture (the symbolic victory lost); Turkey's claim to regional Islamic leadership would weaken; the 1934 secular-nationalist framework would re-establish as governing principle. International heritage advocacy would resume; Orthodox voices would re-emerge in negotiation; non-Muslim access would normalize; geopolitical signaling function (challenging Western heritage regimes) would collapse. The entire political ecosystem the constraint creates would reorganize.
% FOUNDING_PROBLEM: The Turkish Islamic constituency experienced the 1934 secularization (conversion to museum) as erasure of Ottoman legacy and appropriation of Islamic heritage by secular-nationalist elites for cosmopolitan purposes. The constraint addresses the founding problem as: recovery of legitimate Islamic authority over a site consecrated as waqf in 1453, restoration of dignity after centuries of secular dispossession, and reassertion of Turkish state Islamic power against Western-led universal heritage framing.
% FOUNDING_PROBLEM_CORROBORATION: The AKP and Turkish Islamic constituencies affirm the founding problem is live and urgent — the 1934 museum status felt like cultural defeat requiring correction. UNESCO and international heritage advocates (outside the ruling coalition) affirm the founding problem is misframed — the 1934 museum solved a real coordination problem (shared access across religious traditions) and the 2020 decree re-introduces coercive exclusivity. Secular Turkish intellectuals (outside power) affirm the problem was manufactured as political tool by the AKP, not discovered as legitimate grievance. The foundational disagreement: was the 1934 ruling an injustice requiring correction (Islamic reading), or a successful coordination solution requiring preservation (universal heritage reading)? No corroboration from outside the reading's own benefiting parties resolves this — each reading's founding problem is affirmed only by those who benefit from that reading's framing.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint consolidates political power and religious authority in service of AKP electoral interests, not pure coordination. The measurement trajectory is revealing: near-zero in 1934 (museum reading dominates, no extraction under that framing), rising sharply from 2015-2020 as the AKP's Islamization accelerates, and stabilizing at 0.68 post-2020 decree. Suppression is substantial (0.62) because maintaining the constraint requires active state enforcement: police at prayer times, access control, imam appointment, court validation of the decree (reversing 1934 precedent). Theater is moderate-high (0.48) because the constraint performs an identity narrative (Ottoman continuity, Islamic dignity restoration) alongside actual mosque operation; enforcement activity mixes genuine religious function (daily prayer, waqf administration) with state-coercive elements (excluding UNESCO jurisdiction, denying Orthodox restitution claims, suppressing secular objection). Accessibility collapse (0.71) reflects the constraint's structural effect: once the sovereignty framing is established, alternatives (UNESCO museum, interfaith compromise, secular heritage) become politically costly to advocate. Resistance (0.58) models the secular and international opposition that continues but lacks enforcement power within Turkish jurisdiction. The measurement series uses one shared time grid; projected values mark periods before direct observation (Ottoman era, 1934 reversal) and observed values mark measured conditions (2000-2026 post-Atatürk, post-decree era).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (AKP/Turkish state) and the beneficiary (Islamic constituency) experience this as pure coordination — recovery of legitimate Islamic authority and healing of a historical wound. They perceive the 1934 museum ruling as the extraction (secular elites appropriating Islamic heritage for cosmopolitan purposes), and the 2020 decree as correction, not constraint. The victim seats (non-Muslims, UNESCO, secular Turks) experience it as pure extraction — coercive state power mobilized to serve one constituency's identity interests at the cost of shared access, cosmopolitan principle, and competing legitimacy claims. The engine computes per-seat classification from structural data (power, exit, beneficiary/victim placement); the widest seat divergence runs between the institutional-powerful beneficiary seat (AKP) and the powerless-victim seat (non-Muslim visitors), where the same constraint computes as coordination in one and coercion in the other.
 *
 * DIRECTIONALITY LOGIC:
 *   AKP and Turkish Islamic constituency sit near the beneficiary end (d → 0.0): they collect symbolic capital, electoral support, and claim to legitimate authority. Their exit options are weak in the positive direction (they cannot un-win this legitimacy claim) and strong in negative direction (they could lose future elections, but cannot undo the decree). The Orthodox ecclesiastical seat and UNESCO regime have d → 1.0 (full targets of the constraint): their authority is explicitly denied, jurisdiction is stripped, and alternatives are foreclosed. Non-Muslim visitors sit near d → 0.85 (high target): access is substantially constrained; exit exists (visit other sites) but is imperfect. Secular Turkish citizens sit near d → 0.65 (partial target): they experience ideological defeat and symbolic loss, but retain some political voice (opposition parties, civil society) and exit is politically feasible if costly. The Turkish state apparatus that administers the constraint sits near d → 0.5 (symmetric): it bears the cost of maintaining enforcement and international friction, but collects political benefit from the AKP's dominance of state power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (secular nationalist appropriation of Islamic heritage vs. Turkish Islamic dignity) is contested, and the constraint's status as 'solution' or 'extraction' hinges on which reading's legitimacy frame is adopted. From the Islamic sovereignty reading, the 1934 museum conversion was the mandatrophic violation — a secular elite imposing cosmopolitan framing on Islamic space — and the 2020 decree is correction. From the universal heritage reading, the 1934 museum was the successful coordination solution, and the 2020 decree is mandatrophic reversal (recovering a legitimacy claim that should have remained stable). From the Orthodox restitution reading, the 1934 museum was secularist appropriation of both Islamic AND Christian claims, and the 2020 decree is incomplete mandatrophy (correcting toward Islamic but not toward Christian restitution). The constraint story does not resolve this — instead, it documents that mandatrophy is kernel-dependent: which solution solved what problem depends entirely on which legitimacy frame the reader adopts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_baseline_kernel_dependence,
    'Which historical moment establishes the authoritative legitimacy frame for the Hagia Sophia: the 537 Byzantine founding, the 1453 Ottoman conquest, the 1934 secularization, or the 2020 Islamization?',
    'No purely empirical resolution exists — this is a constitutive question about which normative frame the reading adopts. Resolution depends on accepting or rejecting the founding-problem status claims: is the 1934 museum status (UNESCO heritage) legitimate resolution of the contest, or is it secular-nationalist appropriation requiring 2020 correction? No fact resolves the framing choice.',
    'The choice of baseline determines the entire constraint''s classification, beneficiary/victim sets, and extracted-extraction directionality. Islamic sovereignty reading (this one) treats 1453 conquest as baseline, yielding moderate-high extraction and AKP beneficiary. Universal heritage reading treats 1934 secularization as baseline, yielding near-zero extraction (pure coordination). Orthodox restitution reading treats 537 founding as baseline, yielding near-zero extraction (restitution, not extraction). The same site, same facts, three structurally incommensurable readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_baseline_kernel_dependence, conceptual, 'Which historical moment legitimacy is derived from determines the entire reading-specific classification.').

omega_variable(
    suppression_structural_vs_internalized_islamic_identity,
    'Is the suppression of secular Turkish voices and UNESCO jurisdiction structural (external barriers: state police, court decree, institutional closure) or internalized (Turkish citizens have fused their political identity with Islamic nationalism, making exit from the constraint emotionally impossible even if formally feasible)?',
    'Post-constraint dissolution test: if state enforcement were removed and alternative framings (museum, UNESCO, secular heritage) were re-legalized, would secular Turkish citizens and international heritage advocates quickly reorganize political pressure (structural suppression), or would they remain psychologically committed to the Islamic sovereignty reading (internalized identity fusion)? The trajectory of dissent after hypothetical enforcement removal would clarify the mechanism.',
    'If suppression is purely structural, measured suppression (0.62) understates effective suppression because exit remains psychologically feasible. If internalized, the measured suppression is accurate to experienced constraint — targets carry the suppression with them even if formal barriers lift. The distinction affects whether the constraint is Tangled Rope (hybrid coordination/extraction) or shifts toward Snare (pure extraction relying on internalized compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized_islamic_identity, empirical, 'Distinguishes structural coercion (external barriers) from internalized identity fusion in the suppression dynamic.').

omega_variable(
    coordination_function_authenticity_vs_identity_cover,
    'Is the constraint''s coordination function (consolidating Turkish Islamic identity and political coalition) a genuine coordination problem with real beneficiaries, or is it a cover story for pure extraction (AKP consolidation of political power and electoral dominance)?',
    'Comparative institutional test: are there alternative arrangements that could serve the coordination function (Turkish Islamic identity consolidation, regional Sunni leadership signals) without the extraction component (UNESCO exclusion, Orthodox restitution denial, access restriction)? If such alternatives exist and are suppressed, the constraint is primarily extractive with coordination framing. If no alternative serves the function, coordination is genuine.',
    'If coordination is genuine, the constraint is Tangled Rope as claimed — real coordination (identity consolidation) shadowed by asymmetric extraction (access denial, authority suppression). If coordination is cover, the constraint should reclassify to Snare — pure extraction riding on identity narrative. This distinction determines whether the AKP truly benefits from coordination or merely uses coordination as justification for monopolizing access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity_vs_identity_cover, empirical, 'Whether the measured coordination function is genuine problem-solving or post-hoc narrative for extraction.').

omega_variable(
    kernel_contest_committer_ambiguity,
    'This constraint is one reading of a contested kernel (Hagia Sophia substrate). What makes the Islamic sovereignty reading defensible as a freestanding constraint, distinct from the universal heritage and Orthodox restitution readings, is that each reading instantiates genuinely different ε values, different beneficiary/victim structures, and different suppression profiles under the same site. But the three readings are interdependent: the Islamic reading''s ε rises BECAUSE the universal and Orthodox readings exist and are suppressed. Is the extracted-extraction generated by structural asymmetry (this reading truly extracts more than alternatives), or by the kernel-contest dynamics (the constraint''s extractiveness is an artifact of the reading-selection frame)?',
    'Hypothetical-alternative-reading test: if only the Islamic reading existed (universal heritage and Orthodox restitution framings had never been articulated), would the measured ε remain 0.68, or would it fall (because extraction only registers relative to suppressed alternatives)? If ε falls without alternatives to suppress, then extracted-extraction is frame-dependent and not intrinsic to this reading''s structure.',
    'If ε is frame-dependent, the constraint''s classification should be conditioned on the reading-contest landscape: in a multipolar kernel contest, the reading extracts (0.68); in isolation, it would measure as near-zero coordination. This is not a defect of the framework (readings ARE reading-relative), but it clarifies that Tangled Rope status depends on the existence of suppressed alternatives, not on intrinsic structure. The engine computes per-seat classification; it cannot know what alternatives exist in the reading''s own tradition without explicit authoring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_committer_ambiguity, conceptual, 'Whether the constraint''s extractiveness is intrinsic to the reading''s structure or artifact of the kernel-contest landscape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 1453, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1453, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1453, 0.3).
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1934, 0.85).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2000, 0.9).
narrative_ontology:measurement(hagi_tr_t2015, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(hagi_tr_t2026, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1453, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1453, 0.15).
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1934, 0.05).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(hagi_be_t2015, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(hagi_be_t2026, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1453, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1453, 0.2).
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1934, 0.05).
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(hagi_su_t2015, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(hagi_su_t2026, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The Hagia Sophia kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading's legitimacy frame. All three share the physical site and historical facts but diverge on which moment establishes authoritative legitimacy (Byzantine founding, Ottoman conquest, secularization, or Islamization). Islamic sovereignty reading (this story) treats the 1453 conquest and waqf endowment as baseline, yielding moderate-high extraction under Turkish state Islamic authority. Universal heritage reading treats 1934 secularization as baseline, yielding near-zero extraction (coordination solution). Orthodox restitution reading treats 537 founding as baseline, yielding different victim set (non-Orthodox, not non-Muslim) and different extraction profile. The three stories are causally linked: Islamic sovereignty reading's extractiveness (0.68) depends on suppressing the alternatives; removing the alternatives would alter the ε value. All three should be linked via network.affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, powerless, 0.85).
constraint_indexing:directionality_override(hagia_sophia_substrate__islamic_sovereignty_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
