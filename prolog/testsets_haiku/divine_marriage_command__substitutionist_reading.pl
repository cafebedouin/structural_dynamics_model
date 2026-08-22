% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Post-Manifesto Monogamy Doctrine (Substitutionist Reading)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   The substitutionist reading interprets the post-Manifesto prohibition of
 *   polygamy as a new divine revelation that supersedes the prior doctrine
 *   authorizing it. Under this reading, monogamy becomes theologically
 *   required, not merely legally prudent; polygamy transitions from
 *   doctrinally sanctioned to apostasy. This reading legitimates the
 *   institutional shift from prior doctrine by framing it as continuing
 *   revelation, not external coercion. The institutional leadership enforces
 *   this reading through excommunication of fundamentalists who maintain the
 *   prior doctrine, creating a boundary between 'faithful' (substitutionist)
 *   and 'schismatic' (continuationist) practitioners. This constraint
 *   instantiates the temporal coincidence between federal legal suppression
 *   and doctrinal revision as a story about REVELATION, not accommodation —
 *   the most sophisticated cover for extraction: the victims of the shift are
 *   reframed as apostates resisting divine guidance.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Authority claims and excommunication power — defines orthodoxy post-Manifesto
 *   - fundamentalist_practitioners: Maintain prior doctrine, face excommunication, identity-locked in the institution
 *   - polygamist_families: Absorb restructuring costs of existing family arrangements
 *   - compliant_membership: Coordinate around monogamy as boundary marker; benefit from institutional stability
 *   - compliant_theologians: Author interpretive apparatus reconciling prior and new doctrine
 *   - federal_authorities: External coercive pressure, excluded from theological narrative
 *   - coercion_critics: External observers documenting the temporal coincidence between pressure and doctrinal shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Post-Manifesto Monogamy Doctrine (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '318968b8-fe35-47c2-93bf-f1e85cb1ab3c').
narrative_ontology:cs_kernel_codification('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', fixed_text).
narrative_ontology:cs_authority_grounding('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', extraction).
narrative_ontology:cs_interpretation_layer_present('318968b8-fe35-47c2-93bf-f1e85cb1ab3c').
narrative_ontology:cs_reading_relation('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', foundational, continuing_revelation_supersedes_prior_doctrine).
narrative_ontology:cs_axiom_status(continuing_revelation_supersedes_prior_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', continuing_revelation_supersedes_prior_doctrine, deontological).
narrative_ontology:cs_axiom('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', secondary, institutional_leadership_interprets_revelation).
narrative_ontology:cs_axiom_status(institutional_leadership_interprets_revelation, holdable).
narrative_ontology:cs_axiom_grounding('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', institutional_leadership_interprets_revelation, deontological).
narrative_ontology:cs_reference_frame('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', post_manifesto_monogamy_doctrine).
narrative_ontology:cs_drift_state('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', contemporary_fundamentalist_schism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('318968b8-fe35-47c2-93bf-f1e85cb1ab3c', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamist_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, compliant_membership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, compliant_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares the new monogamy doctrine as divinely revealed through continuing revelation, claims authority to interpret scripture's evolution, and administers excommunication of those who maintain prior doctrine. Controls institutional resources, membership criteria, and the legitimacy narrative. Benefits from institutional legal compliance, consolidated authority, and doctrinal uniformity. Frame the shift as spiritual maturation rather than external coercion.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Reject the Manifesto as apostasy and maintain prior doctrine authorizing polygamy. Face excommunication, loss of temple access and community standing, severing of kin networks within the institution, and institutional delegitimation as schismatics. Their theological interpretation is ruled incompatible with membership. Exit means abandoning a religious identity constructed within the institution and formed through decades of community participation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_practitioners, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, fundamentalist_practitioners, excluded).

% Restructure or dissolve existing polygamous marriages to comply with the new doctrine. Bear the emotional, social, and economic costs of family reconfiguration, often separating some spouses from the institutional community or requiring public dissolution of marriages that were religiously valid under prior doctrine. Lack resources to establish competing religious frameworks or legal alternatives.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, polygamist_families, payer,
    powerless, biographical, trapped, regional).

% Retain institutional membership and standing by accepting the new monogamy doctrine. Benefit from institutional resources, temple access, community participation, and social legitimacy. Coordinate around the shared boundary marker (monogamy compliance) that secures institutional coherence against external legal pressure while appearing as doctrinal maturation rather than capitulation to coercion.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, compliant_membership, beneficiary,
    organized, generational, constrained, global).

% Author and teach the interpretive apparatus that reconciles prior and new doctrine, framing the Manifesto as continuing revelation consistent with prior revelation. Build scholarly careers on the substitutionist reading and institutional theological authority. Benefit from platform, resources, and professional reputation tied to the institutional position.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, compliant_theologians, beneficiary,
    powerful, generational, mobile, global).

% Applied legal coercion against polygamy before the Manifesto; their enforcement capacity is the structural condition enabling the doctrinal shift. Remain excluded from the theological narrative, which presents the change as purely internal revelation rather than response to legal pressure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_authorities, excluded,
    institutional, biographical, constrained, national).

% External analysts, historians, and rival denominations document the temporal coincidence between federal legal pressure and the Manifesto's adoption. Argue that the doctrinal shift is accommodationist coercion rationalized as revelation. Their voice is excluded from the institutional legitimacy apparatus.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, coercion_critics, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes monogamy as a unified boundary marker for institutional membership and legitimacy, coordinating the community around a practice the state legally requires. Integrates institutional identity against federal legal pressure by requiring doctrinal uniformity on the marriage question.
% TRANSFER_FUNCTION: Transfers authority over biographical family structure from prior doctrine and individual practitioners' conscience to the institutional leadership's interpretation of continuing revelation. Transfers excommunication capacity and doctrinal boundary-setting power from federal authorities to the institution's disciplinary apparatus. Transfers doctrinal standing from polygamist practitioners to substitutionist theologians.
% ABSENT_VOICES: Fundamentalist practitioners who rejected the Manifesto as apostasy are structurally excluded from the institutional legitimacy apparatus that now defines them as schismatics. Polygamist practitioners whose families face restructuring are excluded from the doctrinal deliberation that required the restructuring. Federal authorities whose legal pressure enabled the shift are excluded from the theological narrative, which presents the change as purely revelatory and internally driven.
% DISAPPEARANCE_RATIONALE: If the monogamy doctrine and its enforcement vanished, the institution would immediately fragment between substitutionist and continuationist factions, fundamentalist schisms would be reintegrated or formally separate, and the mediation between institutional identity and federal legal compliance would be exposed rather than theologized. The institutional boundary between 'faithful' and 'schismatic' would collapse.
% FOUNDING_PROBLEM: Federal legal prohibition of polygamy created existential institutional pressure: prior doctrine authorizing polygamy became legally incompatible with institutional survival in the jurisdiction where the institution held property and members.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily from 0.42 to 0.68 over the interval. At t0 (immediate post-Manifesto), the constraint operates with partial buy-in: compliant members accept the new doctrine, but fundamentalist resistance remains organized and substantial. Theater begins low (0.25) because the coercive function is barely disguised — the federal pressure is visible and acknowledged in institutional discourse. Over time, theater rises to 0.58 as the 'continuing revelation' narrative solidifies: new generations are catechized in substitutionist doctrine as the foundational reading, fundamentalist schisms become historical footnotes rather than live competitors, and the constraint's coercive origins recede into the background. By t=50, extractiveness has plateaued (the initial shock of excommunication and family restructuring is complete) but theater remains elevated — the constraint is now largely performative maintenance of doctrinal orthodoxy. Suppression rises from 0.55 to 0.72 as institutional enforcement machinery hardens: early accommodation for those transitioning is replaced with stricter boundary maintenance. The coercion grid shows individual-level accessibility collapse rising fastest (0.72→0.87): polygamist practitioners see their prior doctrine ruled incompatible with membership, cutting off their exit to institutional resources. Structural-level collapse is slower (0.62→0.75) because the institutional system presents itself as coherent — the constraint is doctrinal, not obviously coercive at the system level. Resistance decays over the interval (0.68→0.64 at individual level; 0.42→0.35 at structural level) as fundamentalists are excommunicated, scattered, or assimilate to avoid persecution.
 *
 * PERSPECTIVAL GAP:
 *   Different seats compute different types because they have structurally different relationships to the constraint. From the institutional leadership seat, the constraint appears as rope (genuine coordination around updated doctrine, minimal coercive overhead). From the fundamentalist and polygamist seats, the same structure appears as tangled_rope or snare (coordination is forced through excommunication; the 'revelation' framing is cover for extraction of doctrinal authority). The engine's per-seat computation captures this: the agenda-setter's d sits near beneficiary; the payer-seats' d sits near target. The claim (tangled_rope) reflects the structural reality: both genuine coordination (around monogamy as boundary marker) AND asymmetric extraction (of prior doctrinal authority, of family autonomy, of fundamentalists' institutional standing) operate through the same constraint. The theater_ratio rising to 0.58 by interval end indicates increasing performative maintenance: the coercive function (enforcing monogamy compliance) is increasingly administratively separate from the coordinative function (boundary maintenance for institutional coherence).
 *
 * DIRECTIONALITY LOGIC:
 *   Fundamentalist practitioners and polygamist families are the structural targets. Their exit is identity-locked: leaving the institution means abandoning the religious identity that was constructed within it, severing networks of kin and community. The institutional leadership benefits by consolidating authority, resolving external legal pressure through apparent internal doctrinal evolution, and establishing excommunication as an enforcement tool. Compliant_theologians benefit through platform and professional authority tied to the institutional position. Federal authorities are structurally excluded but their coercive force is the condition that makes the constraint possible — the doctrine shift would not persist without ongoing federal suppression of polygamy. The directionality is most asymmetric for the powerless polygamist_families (trapped, moderate time horizon, dispersed spatial scope), who face the highest cost-to-benefit ratio and the fewest alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The substitutionist reading presents a critical case for mandatrophy detection. The founding_problem_status is DEAD — federal legal suppression has substantially abated after institutional compliance was demonstrated. The disappearance_verdict is WORLD_REARRANGES — the constraint's removal would fragment the institution. This mismatch (dead problem + rearranging world) is the signature of a constraint whose original function has been satisfied but whose structure persists due to institutional inertia and secondary capture. However, this reading resists simple mandatrophy classification because the institutional leadership has successfully reframed the constraint's origin: rather than 'we changed doctrine under duress,' the narrative is 'God revealed new truth.' If mandatrophy requires honest acknowledgment that the founding problem is resolved and the constraint persists for secondary reasons (inertia, path-dependence, institutional lock-in), then the substitutionist reading CONCEALS mandatrophy by narrativizing coercion as revelation. The constraint is factually mandatrophic (founding problem dead, structure persists) but epistemically resilient to mandatrophy disclosure because the authority apparatus actively rewrites the history. A mandatrophy declaration would require accepting the coercion narrative against the institutional framing — which the constraint's own enforcement machinery works to prevent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_ambiguity,
    'Is the Manifesto a genuine doctrinal evolution through continuing revelation, or a rationalization of institutional accommodation to federal legal pressure?',
    'Archival evidence of institutional deliberation prior to the Manifesto: internal leadership documents discussing the federal pressure, legal risk, and strategic doctrinal revision. Comparative analysis with other institutions'' doctrinal shifts under state pressure. Interview testimony from institutional leadership at the time of the Manifesto.',
    'If the Manifesto is genuinely revelatory (independent of federal pressure), the constraint moves toward rope and the theater_ratio drops. If the Manifesto is transparent accommodation rationalized as revelation, the constraint remains tangled_rope and theater_ratio stays elevated, confirming the coercive mechanism is being actively disguised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion_ambiguity, empirical, 'Whether the doctrinal shift is revelation or coercion rationalization').

omega_variable(
    continuing_revelation_framework_authority,
    'What criteria distinguish a valid continuing revelation from institutional rewriting of doctrine under external pressure, within the theological framework that accepts continuing revelation?',
    'Theological analysis: what textual or doctrinal resources does this reading deploy to establish that the Manifesto meets the criteria for continuing revelation? Are those criteria applied consistently to other claimed revelations, or do they appear constructed post-hoc to legitimize the Manifesto specifically?',
    'If the criteria are general and consistently applied, the continuing revelation framework has real gatekeeping force. If the criteria appear post-hoc or inconsistently applied, the framework is functioning as cover for institutional authority rather than as a genuine epistemic test. This affects whether the constraint''s legitimacy is theologically robust or merely procedurally enacted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuing_revelation_framework_authority, conceptual, 'Whether the continuing revelation framework has independent gatekeeping force or is instrumentally applied to legitimize institutional decisions').

omega_variable(
    identity_lock_durability_post_excommunication,
    'How deeply is the fundamentalist practitioner''s religious identity locked to institutional membership? What fraction of identity-lock persists after excommunication, and what fraction is conditional on institutional access?',
    'Longitudinal study of fundamentalists post-excommunication: do they maintain religious identity and practice outside the institution? Do they form separate communities? How much psychological distress attends the separation, and does it persist or diminish over time? Comparison with excommunicates from other institutions.',
    'If identity-lock is deep and persists post-excommunication, the constraint''s suppression of fundamentalism is highly effective and the institutional cost is paid as internalized identity conflict. If identity-lock is shallow and excommunicates readily form separate communities, the institutional suppression has lower effectiveness and higher open resistance. This affects the measured suppression and resistance values.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_durability_post_excommunication, empirical, 'The depth and durability of identity-lock among fundamentalist practitioners post-excommunication').

omega_variable(
    institutional_coordination_necessity_of_monogamy_boundary,
    'Is monogamy structurally necessary for institutional coordination under federal legal suppression, or would an alternative boundary marker (e.g., public legal compliance + private doctrinal tolerance) accomplish the coordination without requiring the doctrinal shift?',
    'Natural experiment from historical institutions that maintained separate doctrinal and legal stances. Institutional modeling: what alternative arrangements could coordinate the membership and satisfy federal authorities while preserving doctrinal continuity?',
    'If monogamy is necessary for coordination, the constraint''s coordination function is genuine and substantial extraction is a cost of maintaining institutional coherence. If alternative arrangements are possible, the monogamy requirement is unnecessary coercion dressed as necessary coordination, increasing the extractiveness measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_coordination_necessity_of_monogamy_boundary, conceptual, 'Whether the monogamy boundary is structurally necessary for institutional coordination or is one option among viable alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__substitutionist_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(divi_tr_t35, divine_marriage_command__substitutionist_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement(divi_tr_t50, divine_marriage_command__substitutionist_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__substitutionist_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(divi_be_t35, divine_marriage_command__substitutionist_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(divi_be_t50, divine_marriage_command__substitutionist_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__substitutionist_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(divi_su_t35, divine_marriage_command__substitutionist_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(divi_su_t50, divine_marriage_command__substitutionist_reading, suppression_requirement, 50, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(divi_grid_01, divine_marriage_command__substitutionist_reading, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(divi_grid_02, divine_marriage_command__substitutionist_reading, accessibility_collapse(class), 50, 0.79).
narrative_ontology:measurement(divi_grid_03, divine_marriage_command__substitutionist_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(divi_grid_04, divine_marriage_command__substitutionist_reading, accessibility_collapse(individual), 50, 0.87).
narrative_ontology:measurement(divi_grid_05, divine_marriage_command__substitutionist_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(divi_grid_06, divine_marriage_command__substitutionist_reading, accessibility_collapse(organizational), 50, 0.84).
narrative_ontology:measurement(divi_grid_07, divine_marriage_command__substitutionist_reading, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(divi_grid_08, divine_marriage_command__substitutionist_reading, accessibility_collapse(structural), 50, 0.75).
narrative_ontology:measurement(divi_grid_09, divine_marriage_command__substitutionist_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(divi_grid_10, divine_marriage_command__substitutionist_reading, resistance(class), 50, 0.52).
narrative_ontology:measurement(divi_grid_11, divine_marriage_command__substitutionist_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(divi_grid_12, divine_marriage_command__substitutionist_reading, resistance(individual), 50, 0.64).
narrative_ontology:measurement(divi_grid_13, divine_marriage_command__substitutionist_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(divi_grid_14, divine_marriage_command__substitutionist_reading, resistance(organizational), 50, 0.58).
narrative_ontology:measurement(divi_grid_15, divine_marriage_command__substitutionist_reading, resistance(structural), 0, 0.42).
narrative_ontology:measurement(divi_grid_16, divine_marriage_command__substitutionist_reading, resistance(structural), 50, 0.35).
narrative_ontology:measurement(divi_grid_17, divine_marriage_command__substitutionist_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(divi_grid_18, divine_marriage_command__substitutionist_reading, stakes_inflation(class), 50, 0.71).
narrative_ontology:measurement(divi_grid_19, divine_marriage_command__substitutionist_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(divi_grid_20, divine_marriage_command__substitutionist_reading, stakes_inflation(individual), 50, 0.82).
narrative_ontology:measurement(divi_grid_21, divine_marriage_command__substitutionist_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(divi_grid_22, divine_marriage_command__substitutionist_reading, stakes_inflation(organizational), 50, 0.76).
narrative_ontology:measurement(divi_grid_23, divine_marriage_command__substitutionist_reading, stakes_inflation(structural), 0, 0.45).
narrative_ontology:measurement(divi_grid_24, divine_marriage_command__substitutionist_reading, stakes_inflation(structural), 50, 0.68).
narrative_ontology:measurement(divi_grid_25, divine_marriage_command__substitutionist_reading, suppression(class), 0, 0.55).
narrative_ontology:measurement(divi_grid_26, divine_marriage_command__substitutionist_reading, suppression(class), 50, 0.71).
narrative_ontology:measurement(divi_grid_27, divine_marriage_command__substitutionist_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(divi_grid_28, divine_marriage_command__substitutionist_reading, suppression(individual), 50, 0.74).
narrative_ontology:measurement(divi_grid_29, divine_marriage_command__substitutionist_reading, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(divi_grid_30, divine_marriage_command__substitutionist_reading, suppression(organizational), 50, 0.72).
narrative_ontology:measurement(divi_grid_31, divine_marriage_command__substitutionist_reading, suppression(structural), 0, 0.42).
narrative_ontology:measurement(divi_grid_32, divine_marriage_command__substitutionist_reading, suppression(structural), 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel is decomposed into three readings instantiating three distinct constraints with different ε values and different stakeholder structures. The substitutionist reading (this constraint) reports high extractiveness (0.68) and high theater (0.58) because it observes the Manifesto as coercive enforcement rationalized as revelation. The continuationist reading would report lower extractiveness (the imposed monogamy is temporary accommodation, not true doctrine) and different suppression (directed against continuationists, not fundamentalists). The coercion_visibility reading would report lower theater (coercion is acknowledged rather than rationalized) and different beneficiary/victim structures (beneficiary is survival legitimacy, not doctrinal authority). All three share the referent (post-Manifesto monogamy requirement) and differ in ε-value, stakeholder roles, and the authority narrative. Linked via network.affects_constraints for constraint-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
