% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__cultural_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Jewish Cultural Renaissance and Spiritual Center in Palestine (Cultural Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The cultural-zionist reading frames Jewish presence in Palestine as a
 *   cultural and spiritual renaissance project, emphasizing the revival of
 *   Hebrew language, establishment of educational and religious institutions,
 *   and reconnection to historical and archaeological identity — without
 *   requiring political sovereignty or demographic majority. This reading
 *   presents the arrangement as cooperative cultural coexistence with
 *   Palestinian inhabitants, positioning Palestinians as co-inhabitants in a
 *   shared territorial and intellectual space rather than as obstacles to be
 *   displaced or subordinated. The cultural-zionist movement (Ahad Ha'am,
 *   early institutional builders) explicitly rejected maximalist territorial
 *   claims and sovereignty as the sole or primary goal, instead envisioning
 *   cultural autonomy as the core benefit. This reading instantiates one pole
 *   of a fundamental contest over what Zionism is and whether it requires,
 *   enables, or is compatible with Palestinian self-determination in the same
 *   territory.
 *
 * KEY AGENTS:
 *   - Jewish diaspora communities — globally dispersed but organizationally coherent; seek cultural vitality and linguistic continuity without claiming political control
 *   - Palestinian Arab inhabitants — existing and future; positioned in this reading as co-inhabitants and co-designers of shared institutions, with territorial security guarantees
 *   - Hebrew language movement — cultural practitioners and educators seeking institutional support for language revival
 *   - Ottoman/Mandate administrators — agenda-setters under colonial framework; permit cultural development within defined boundaries
 *   - Rival Zionist readings (liberal-nationalist, religious, settler-colonial) — excluded from this reading's design table; their interpretation of the same facts diverges sharply
 *   - Arab and Palestinian nationalists — excluded voices who contest the entire premise of peaceful coexistence with a Jewish territorial center
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.28).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.15).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Jewish Cultural Renaissance and Spiritual Center in Palestine (Cultural Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, '6f2a8afb-2016-482e-a9af-53a1a39287f7').
narrative_ontology:cs_kernel_codification('6f2a8afb-2016-482e-a9af-53a1a39287f7', distributed).
narrative_ontology:cs_authority_grounding('6f2a8afb-2016-482e-a9af-53a1a39287f7', distributed).
narrative_ontology:cs_reading_relation('6f2a8afb-2016-482e-a9af-53a1a39287f7', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f2a8afb-2016-482e-a9af-53a1a39287f7', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('6f2a8afb-2016-482e-a9af-53a1a39287f7', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f2a8afb-2016-482e-a9af-53a1a39287f7', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('6f2a8afb-2016-482e-a9af-53a1a39287f7', foundational, jewish_cultural_vitality_without_sovereignty).
narrative_ontology:cs_axiom_status(jewish_cultural_vitality_without_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6f2a8afb-2016-482e-a9af-53a1a39287f7', jewish_cultural_vitality_without_sovereignty, instrumental).
narrative_ontology:cs_axiom('6f2a8afb-2016-482e-a9af-53a1a39287f7', foundational, palestinian_arab_coexistence_compatibility).
narrative_ontology:cs_axiom_status(palestinian_arab_coexistence_compatibility, holdable).
narrative_ontology:cs_axiom_grounding('6f2a8afb-2016-482e-a9af-53a1a39287f7', palestinian_arab_coexistence_compatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('6f2a8afb-2016-482e-a9af-53a1a39287f7', pluralist_cultural_autonomy_without_territorial_maximalism).
narrative_ontology:cs_drift_state('6f2a8afb-2016-482e-a9af-53a1a39287f7', contemporary_post_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f2a8afb-2016-482e-a9af-53a1a39287f7', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_language_speakers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diaspora Jewish communities gain a cultural and spiritual center for Hebrew revival, archaeological connection, and institutional development of Jewish cultural institutions (universities, theaters, museums, libraries, religious academies). They contribute resources and migration without requiring displacement of existing inhabitants; the arrangement offers cultural vitality and continuity without zero-sum sovereignty claims.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, arbitrage, global).

% Hebrew language revitalization benefits from institutional support, education systems, publishing, and media in a territorial center dedicated to the language. Speakers gain a functioning linguistic ecosystem and cultural prestige without requiring the language to displace Arabic as the regional norm.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_language_speakers, beneficiary,
    moderate, biographical, mobile, regional).

% Scholars, artists, religious leaders, and intellectuals engaged in Jewish cultural and spiritual traditions establish institutions, schools, and centers of learning in Palestine. They benefit from concentrated cultural infrastructure and community patronage; the arrangement supports institutional development without requiring political sovereignty.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_practitioners, beneficiary,
    moderate, biographical, constrained, regional).

% In this reading's framework, Palestinians are positioned as co-inhabitants in a shared territorial and cultural space. The arrangement envisions Jewish cultural institutions and Hebrew revival operating in parallel with Arab-Palestinian institutions and Arabic cultural life. Palestinians benefit from cross-cultural exchange, economic opportunity in shared infrastructure, and territorial security guarantees; costs are diffuse (cultural competition, resource allocation negotiation, demographic pressure on housing and services).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_inhabitants, payer).

% Under Ottoman and later British Mandate frameworks, administrators permit and sometimes facilitate Jewish cultural and institutional development in Palestine (educational institutions, land acquisition for communal purposes, cultural societies). They enforce minimal rules protecting both Jewish and Arab property and community interests.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, ottoman_and_colonial_administrators, agenda_setter,
    institutional, biographical, trapped, regional).

% Arab nationalist movements, Palestinian autonomy advocates, and regional powers that frame Jewish presence as incompatible with Arab sovereignty are structurally excluded from the cultural-zionist framework's negotiation table. They would argue that cultural autonomy without political sovereignty is a mask for subordination; their voice in the design of the arrangement is absent.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, rival_nationalist_movements, excluded,
    powerful, generational, constrained, regional).

% Different Jewish religious communities and authorities evaluate the cultural-zionist reading through competing theological lenses. Some view cultural revival in the ancestral land as a step toward redemption; others see it as a secular appropriation of religious symbolism. Their assessments shape diaspora willingness to support the arrangement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_religious_authorities, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_communities).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__cultural_zionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a territorial cultural and intellectual center for the revival and institutional development of Hebrew language, Jewish scholarship, religious study, and artistic traditions. Solves the coordination problem of maintaining diaspora cultural identity across dispersed communities by providing a concentrated, shared institutional infrastructure and symbolic homeland for cultural practices.
% TRANSFER_FUNCTION: Moves diaspora resources (capital, migration, intellectual labor) to Palestine to establish educational, religious, and cultural institutions. In return, diaspora communities receive cultural vitality, linguistic continuity, institutional prestige, and spiritual connection. Palestinians provide territorial hospitality and co-existence; in this reading's framework, they receive economic participation and security guarantees.
% ABSENT_VOICES: Arab and Palestinian nationalists who view any Jewish territorial concentration as an existential threat, regardless of stated intent; Zionist readers (liberal nationalist, religious, settler-colonial) who read the same facts entirely differently; Jewish labor movements and secular radicals who reject cultural nationalism as bourgeois mystification; European colonial powers whose interests in maintaining Palestine as a territorial sphere diverge from both Jewish and Arab autonomous development.
% DISAPPEARANCE_RATIONALE: The cultural-zionist reading claims that Jewish cultural institutions and Hebrew revival would contract significantly if Palestinian co-inhabitance was genuinely secured through power-sharing and legal equality (the arrangement depends on actual Palestinian acceptance, not subordination). Palestinian nationalists argue the arrangement would collapse because Jewish territorial concentration is inherently destabilizing; settler-colonial readers argue it would shift to coercive sovereignty within a generation. The verdict depends on whether the cultural function is genuinely separable from political domination.
% FOUNDING_PROBLEM: Jewish diaspora communities face assimilation, cultural erosion, and loss of linguistic continuity across generations of dispersion. Simultaneously, Jewish scholarship, spirituality, and artistic traditions require institutional support and territorial rootedness to sustain themselves. The arrangement attempts to provide a cultural and spiritual center that revitalizes these traditions without requiring the displacement of existing inhabitants.
% FOUNDING_PROBLEM_CORROBORATION: Jewish cultural and intellectual leaders (Ahad Ha'am, Martin Buber, Gershom Scholem, contemporary Israeli authors and thinkers) attest the founding problem and endorse the cultural-zionist reading as a legitimate response. Palestinian scholars and post-Zionist Israeli academics contest whether the founding problem can be solved in a territory already inhabited by another people without either domination or displacement. No external observer within the region has attested the founding problem as solved by the cultural-zionist arrangement; regional and international readings diverge sharply on the empirical question of whether cultural autonomy is operationally separable from political sovereignty.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, contested).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.28 (interval end) because the arrangement extracts resources from diaspora communities into Palestine, but does not extract political submission or territorial displacement from Palestinians — the extraction is consensual (in the reading's framework) or at least non-coercive in the core dimension. Suppression is low (0.15) because the arrangement, as stated, requires no active suppression of Palestinian alternatives or alternatives to coexistence; it assumes negotiated boundaries and institutional power-sharing. Theater ratio is minimal (0.10) because the cultural function is stated as primary throughout, not a cover story for sovereignty accumulation (though rival readings dispute this). Accessibility_collapse is moderate (0.35) because while alternatives to cultural coexistence exist (assimilation, Hebrew death, territorial separation), the arrangement claims to offer something sufficiently valuable that it need not collapse alternatives through coercion. Resistance is substantial (0.42) because multiple stakeholder groups contest the reading: Arab nationalists reject it on principle, liberal Zionists see it as insufficiently protective of Jewish political power, religious Zionists see it as insufficiently rooted in divine claim, and post-Zionists see it as a transitional ideology that will collapse into either sovereignty or displacement. The measurement series shows extractiveness and suppression rising modestly over time (from 0.12/0.08 to 0.28/0.15), which the omega variables interpret as the reality-testing of the coexistence assumption — as time passes and actual resource allocation, housing pressure, and institutional competition increases, the measured extraction rises, though it remains well below snare or tangled_rope thresholds.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (Palestinians as co-inhabitants, secondary role) and the beneficiary seat (diaspora, primary role) should diverge sharply when the engine computes per-seat classifications. From the diaspora seat, the arrangement is a rope solving genuine coordination problems with minimal coercion. From the Palestinian seat, the same structural facts may resolve as tangled rope (coordination + asymmetric extraction) or even incipient snare, depending on whether Palestinian voice in institutional design is genuine. The schema treats these as separate seats with different power/exit profiles; the divergence is where the committer frame's signal lives.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities have low directionality (d near 0.0, beneficiary end) because they collect cultural vitality without bearing suppression costs or territorial displacement. Hebrew speakers have moderate-low directionality because they benefit from institutional support but face ongoing assimilation pressure. Jewish cultural practitioners benefit similarly. Palestinians in this reading are positioned as symmetric to low-beneficiary (d around 0.3–0.4): they receive economic participation and cultural exchange but pay diffuse costs of demographic pressure, resource competition, and the risk that coexistence assumptions prove incorrect. Ottoman/Mandate administrators have moderate directionality because they benefit from political stability and reduced conflict (lower administration cost) but pay the cost of maintaining enforcement of boundaries. Rival nationalist movements (Arab, regional Zionist variants) are excluded — their directionality cannot be computed from this framework because they are not seated as participants. The overage in suppression_requirement relative to base_extractiveness (0.15 vs 0.28) reflects the energy required to maintain the coexistence assumption in the face of nationalist pressure from all sides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora cultural erosion, linguistic discontinuity) and its stated resolution (territorial cultural center with Palestinian coexistence) remain in structural tension throughout the interval. The measurement series suggests that the tension does not resolve: extractiveness and suppression rise over time even though the cultural function (revitalization, institutional development) should be more fully achieved. This pattern indicates either (a) the coexistence assumption degrades under demographic and resource pressure, requiring increasing suppression to maintain, or (b) the founding problem shifts from cultural survival to political power as time passes and cultural institutions solidify. The mandatrophy is incipient in the interval but not fully resolved — the reading preserves the cultural narrative while the material facts drift toward sovereignty demands. A post-Zionist or liberal-nationalist reading would interpret this drift as validation of their reading's superiority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_autonomy_vs_political_domination,
    'Can Jewish cultural and institutional development in Palestine genuinely occur without requiring political sovereignty or demographic majority, or is cultural presence structurally coupled to political domination such that separation is empirically impossible?',
    'Comparative case analysis of diaspora cultural centers without territorial sovereignty (Vatican City model, European minority cultural institutions, diaspora university networks); examination of whether cultural-only arrangements have persisted without either cultural erosion or political subordination of neighbors.',
    'If genuinely separable, the cultural-zionist reading''s ε remains low and the constraint is legitimately a rope. If coupled, ε rises substantially and the reading becomes a cover story for sovereignty extraction — reclassifying toward snare or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_autonomy_vs_political_domination, empirical, 'Whether cultural autonomy is operationally separable from political sovereignty in contested territorial contexts.').

omega_variable(
    palestinian_coexistence_framework_credibility,
    'Is the framework of Palestinians as co-inhabitants in a shared cultural space with genuine institutional voice realistic, or does the power asymmetry (diaspora organizational capacity, external support) make subordination inevitable?',
    'Analysis of documented Palestinian participation in institutional design (were Palestinians consulted as co-designers or presented with a completed framework?); comparison with other cases of pluralist coexistence in asymmetric power contexts.',
    'If Palestinians are genuinely co-designers and hold institutional veto power, the arrangement''s extractiveness remains low. If Palestinians are presented with a fait accompli or hold only advisory status, suppression rises and the arrangement drifts toward coercive asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_coexistence_framework_credibility, empirical, 'Whether Palestinians have genuine voice in designing the shared institutional framework, or are merely recipients of a Jewish-designed arrangement.').

omega_variable(
    diaspora_intent_vs_settler_colonial_dynamics,
    'Does the cultural-zionist reading instantiate a genuinely pluralist, non-territorial-maximalist movement, or does the settler-colonial reading correctly identify it as the entry phase of displacement dynamics that become apparent only over generational timescales?',
    'Historical trajectory analysis: do cultural-zionist institutions remain bounded and constrained to agreed territories, or do they expand territorially and demographically such that political sovereignty follows within a century? Examination of stated intent vs. institutional behavior across the full interval.',
    'If cultural bounds hold, the reading''s characterization as cooperative cultural development is validated. If territorial expansion follows cultural establishment, the settler-colonial reading''s framing becomes empirically vindicated and the constraint reclassifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_intent_vs_settler_colonial_dynamics, empirical, 'Whether cultural-zionist dynamics remain bounded or generate the expansion pressures that settler-colonial theory predicts.').

omega_variable(
    kernel_reading_underdetermination,
    'Is the distinction between the cultural-zionist reading (cultural center without sovereignty) and the liberal-nationalist reading (self-determination and statehood as intrinsic rights) a substantive structural difference, or does it merely reflect different foundational axioms about whether collective self-determination requires territorial political sovereignty?',
    'Philosophical analysis of the two readings'' core premises: if they differ only in whether political statehood is necessary for cultural flourishing, they are axiomatically distinguished but empirically may converge. If they differ in the beneficiary structure (shared vs. exclusive sovereignty) and Palestinian role (co-designer vs. excluded), they are structurally distinct constraints.',
    'If axiomatically distinguished but empirically convergent, the readings should coexist; if structurally distinct, foreclosure relations may apply. The cs_structure.axioms field clarifies this for engine computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the cultural-zionist and liberal-nationalist readings differ at the level of substantive structure or only at the level of foundational normative commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(jewi_tr_t15, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(jewi_tr_t50, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 50, 0.11).
narrative_ontology:measurement(jewi_tr_t75, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement(jewi_tr_t100, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(jewi_be_t15, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(jewi_be_t50, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(jewi_be_t75, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 75, 0.29).
narrative_ontology:measurement(jewi_be_t100, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(jewi_su_t15, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(jewi_su_t50, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 50, 0.16).
narrative_ontology:measurement(jewi_su_t75, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 75, 0.17).
narrative_ontology:measurement(jewi_su_t100, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__cultural_zionist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% The jewish_sovereignty_palestine kernel decomposes into five structurally distinct constraints, one per reading. Each reading instantiates a different ε (beneficiary/victim structure) and classification. The cultural-zionist reading (this file) claims rope with low extractiveness; the settler-colonial reading claims snare with high extractiveness evaluated from the same territorial facts. They are not different observations of one constraint — they are different constraints instantiated by different readings of a shared kernel. All five readings share network edges to document their mutual influence and foreclosure relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__cultural_zionist_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
