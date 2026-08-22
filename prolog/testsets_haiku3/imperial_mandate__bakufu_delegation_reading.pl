% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bifurcated Imperial Sovereignty: Bakufu Delegation Reading
 *   domain: political/constitutional/institutional
 *
 * SUMMARY:
 *   The bakufu delegation reading instantiates one structural interpretation
 *   of the imperial mandate: divine authority operates through institutional
 *   delegation, bifurcating the emperor's legitimacy-granting function
 *   (ritual, symbolic, mandate-holding) from the shogun's
 *   authority-exercising function (administrative, military, enforcement).
 *   Under this reading, the emperor grants legitimacy to successive shoguns,
 *   enabling regime transitions to preserve institutional continuity while
 *   practical governance evolves. The samurai class governs through the
 *   bakufu, excluding other classes and suppressing imperial political
 *   autonomy. The claim is Tangled Rope: genuine coordination problem
 *   (legitimacy-authority bifurcation) combined with asymmetric extraction
 *   (samurai monopoly, imperial suppression, daimyo constraint). The
 *   measurement series tracks rising extractiveness and theater_ratio over 30
 *   periods, indicating the coordinating function decayed over time while the
 *   extractive function persisted and required increasing theatrical
 *   maintenance.
 *
 * KEY AGENTS:
 *   - Emperor: Holds mandate, grants legitimacy through delegation, suppressed from political exercise (identity-locked, institutional power, generational horizon)
 *   - Shogun/Bakufu: Exercises delegated administrative authority, dependent on imperial legitimacy (institutional power, constrained exit)
 *   - Samurai governing class: Benefits from monopoly on legitimate administrative power (organized, identity-locked to governing hierarchy)
 *   - Daimyo regional lords: Constrained regional autonomy, subordinate to bakufu (powerful, but subject to suppression)
 *   - Imperial court bureaucracy: Maintains rituals and ceremonies but withheld from political authority (moderate power, identity-locked)
 *   - Merchant/artisan classes: Excluded from legitimate political voice despite economic power (powerful economically, politically excluded)
 *   - Confucian intellectual tradition: Provides justification for hierarchical bifurcation (vindicated proposition, not agent)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.72).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.43).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bifurcated Imperial Sovereignty: Bakufu Delegation Reading").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political/constitutional/institutional").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '5a4adf73-9782-43cc-af58-c856fb2e8cc0').
narrative_ontology:cs_kernel_codification('5a4adf73-9782-43cc-af58-c856fb2e8cc0', distributed).
narrative_ontology:cs_authority_grounding('5a4adf73-9782-43cc-af58-c856fb2e8cc0', lineage).
narrative_ontology:cs_interpretation_layer_present('5a4adf73-9782-43cc-af58-c856fb2e8cc0').
narrative_ontology:cs_reading_relation('5a4adf73-9782-43cc-af58-c856fb2e8cc0', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('5a4adf73-9782-43cc-af58-c856fb2e8cc0', foundational, legitimacy_authority_bifurcation_valid).
narrative_ontology:cs_axiom_status(legitimacy_authority_bifurcation_valid, holdable).
narrative_ontology:cs_axiom_grounding('5a4adf73-9782-43cc-af58-c856fb2e8cc0', legitimacy_authority_bifurcation_valid, conventional).
narrative_ontology:cs_axiom('5a4adf73-9782-43cc-af58-c856fb2e8cc0', secondary, samurai_exclusive_governance).
narrative_ontology:cs_axiom_status(samurai_exclusive_governance, holdable).
narrative_ontology:cs_axiom_grounding('5a4adf73-9782-43cc-af58-c856fb2e8cc0', samurai_exclusive_governance, conventional).
narrative_ontology:cs_reference_frame('5a4adf73-9782-43cc-af58-c856fb2e8cc0', bifurcated_imperial_sovereignty).
narrative_ontology:cs_drift_state('5a4adf73-9782-43cc-af58-c856fb2e8cc0', late_bakufu_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a4adf73-9782-43cc-af58-c856fb2e8cc0', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogun_administrative_authority).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court_political_autonomy).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, daimyo_regional_independence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, emperor).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, daimyo_regional_lords).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the mandate of heaven and grants legitimacy through ritual function and official appointment of the shogun. Receives institutional deference, ceremonial status, and the benefit that imperial institution persists across regime changes through the delegation mechanism. Political involvement in day-to-day governance is formally suppressed by the arrangement; the emperor's authority is confined to legitimacy-granting. Exit from the role is impossible without dissolving the imperial institution itself, making the emperor's identity constitutive of and coterminous with the constraint.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, emperor, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, emperor, beneficiary).

% Exercises day-to-day governing authority, law enforcement, military command, and administrative policy-making. Legitimacy for this authority derives from imperial appointment/delegation; the shogun does not govern in the emperor's name as a proxy but receives delegated authority directly from the throne. Operates independently from imperial direction in practical governance; the suppression of imperial political autonomy is necessary to maintain the shogun's autonomous administrative authority. Could attempt to eliminate imperial authority entirely, but doing so would destroy the legitimacy the delegation provides.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogun_administrative_authority, agenda_setter,
    institutional, generational, constrained, national).

% Governs through the bakufu administrative apparatus and military structure under the shogun's command. Benefits from exclusive, legally-entrenched monopoly on legitimate political authority and administrative office. Samurai identity is constituted through role in the hierarchical governing structure; samurai honor, training, and legitimacy flow from their position in the delegated authority chain. The arrangement protects this monopoly by suppressing both imperial political action (which would disrupt the hierarchy) and merchant/artisan claims to voice (which would dilute samurai exclusivity). Cannot exit the arrangement without ceasing to be samurai.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_governing_class, beneficiary,
    organized, generational, identity_locked, national).

% Retain regional authority within their domains but are subordinate to the bakufu and shogun in all matters of national policy and military command. Required to acknowledge imperial legitimacy and shogunal authority; forbidden from independent foreign relations or military action. Bear the cost of constrained autonomy while the samurai class (drawn from daimyo families but acting through bakufu) retain governing monopoly. Rebellion is suppressed militarily; peaceful autonomy assertion is blocked by institutional structure. Regional autonomy is the extraction mechanism.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, daimyo_regional_lords, payer,
    powerful, generational, constrained, national).

% Serves the emperor in ceremonial, ritual, and symbolic administrative functions (court protocol, shrine maintenance, regional appointment ceremonies). Executes imperial authority in its narrow, legitimacy-granting form but is barred from political decision-making or policy implementation. Court members' status and career depend entirely on imperial institution preservation; they cannot exit without losing institutional identity. Bear the cost of suppressed political autonomy while maintaining the rituals that legitimize the bakufu.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court_bureaucracy, payer,
    moderate, generational, identity_locked, local).

% Possess economic power and organizational capacity but are structurally excluded from legitimate political participation and governing authority. Would argue for merit-based advancement, political representation, and translated economic power into voice, but the samurai monopoly and imperial legitimacy structure prevents any formal inclusion. Can influence policy indirectly through merchants' associations, but have no seat at decision-making tables. Economic subordination is enforced by law (status codes) and institutional practice.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, merchant_artisan_classes, excluded,
    powerful, biographical, constrained, regional).

% The bifurcated delegation reading instantiates and is vindicated by a particular Confucian interpretation: that hierarchy is natural, that ritual and moral authority are superior to administrative authority, that the emperor's role in granting legitimacy through virtue and ceremony is the apex of authority even though practical governance is delegated. Intellectual justification for the arrangement flows through Confucian frameworks; the reading treats bifurcation as the harmonious ordering of society and as the natural expression of mandate principles. Not an agent with interests but a non-agent entity that benefits from the constraint's operation vindicating its worldview.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, confucian_intellectual_class, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(imperial_mandate__bakufu_delegation_reading, confucian_intellectual_class).

% Analyze and document the bakufu arrangement's operation, legitimacy claims, and actual practice. Can measure the gap between declared bifurcation (legitimacy delegated) and operational reality (suppression of imperial autonomy). Their analysis shapes how the constraint is understood and can challenge the coordination narrative in favor of a suppression reading. No direct stake in the arrangement's continuance; role is to understand and measure.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, historians_and_political_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of institutional continuity during succession of governing authority: by separating legitimate authority-granting (emperor) from practical authority-exercising (shogun), regime transitions can occur smoothly without institutional collapse. A new shogun receives imperial appointment and thus inherits legitimacy; the imperial institution persists unchanged across shogunal turnover. Coordinates between security needs (decentralized samurai military in early period) and administrative coherence (centralized bakufu policy). Solves the problem of justifying exclusionary samurai monopoly: the exclusivity flows from imperial delegation to the warrior class, grounding class hierarchy in cosmological order rather than military conquest.
% TRANSFER_FUNCTION: Moves political autonomy FROM the imperial court (confined to ritual legitimacy-granting), FROM daimyo (constrained to regional dominion under bakufu direction), FROM merchant and artisan classes (excluded from governing altogether) TO the shogun and samurai class. The constraint channels political power exclusively through the samurai-administered bakufu. Imperial court surrenders practical political authority in exchange for institutional preservation and ceremonial status. Daimyo surrender national-level authority in exchange for regional autonomy maintained (not guaranteed). Merchants surrender any claim to political voice in exchange for commercial operation under samurai law.
% ABSENT_VOICES: Merchant and artisan classes are excluded from the conversation; they would argue for merit-based advancement, political representation proportional to economic power, and abolition of samurai monopoly. Imperial court factions advocating for restoration of direct political authority are suppressed and silenced; court members cannot voice political ambitions without institutional ostracism. Regional daimyo advocating for restoration of autonomous power are constrained by military subordination; their voices are heard only in private and are overridden at the bakufu center. Confucian scholars outside the orthodoxy who question whether bifurcation is the natural order are marginalized. The voices excluded are precisely those that would challenge the samurai monopoly and the suppression of imperial autonomy.
% DISAPPEARANCE_RATIONALE: If the bifurcated delegation constraint evaporated overnight, multiple institutional reorganizations would cascade: (1) the imperial court would immediately assert political authority and expand its bureaucratic apparatus; (2) daimyo would recover autonomous military and policy-making capacity in their domains; (3) merchant classes would push for political voice and begin to gain appointments to decision-making councils; (4) samurai monopoly would break as other classes entered governing roles; (5) the shogunate itself might dissolve or become a merely administrative office under imperial sovereignty. The arrangement is not a natural feature of governance that would reassert itself but a constructed institutional settlement that requires continuous maintenance through suppression and theatrical legitimacy-reaffirmation.
% FOUNDING_PROBLEM: Late Heian and Kamakura transition period: the imperial court's administrative capacity collapsed; regional military powers (daimyo and their samurai retainers) became necessary for security and territorial control; required a structural accommodation that would preserve imperial legitimacy while delegating practical governing authority to warrior-administrators capable of military enforcement and regional administration.
% FOUNDING_PROBLEM_CORROBORATION: Bakufu-era historians, Confucian scholars, and shogunal court records attest the founding problem was genuine: court militaries had failed, central authority had collapsed into regional warlordism, and the bifurcated delegation was a pragmatic solution. Modern historians outside the bakufu tradition document that the need for decentralized samurai military capacity persisted only through the early shogunal period (2-3 centuries) and then became obsolete; yet the samurai monopoly and suppression mechanisms persisted for centuries beyond the founding problem's dissolution. Loyalist restoration-era historians and court-sympathetic observers argue the founding problem reflects only the court's administrative incompetence and that the delegation became a cover story for military usurpation; they attest the problem was never as severe as bakufu sources claim. Contemporary observers and foreign analysts document that by the late bakufu era, the founding problem (security via decentralized samurai) no longer existed, yet the theater of delegation required increasing enforcement and suppression to maintain.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.45) when the arrangement is functional: the coordination problem is real, regime transitions are managed smoothly, security is provided. Over 30 periods, extractiveness rises to 0.68 as the founding problem (need for samurai military) becomes obsolete but the arrangement persists. Theater_ratio rises from 0.28 to 0.51, indicating that an increasing share of the arrangement's enforcement activity becomes performative: maintaining the fiction of delegation despite the underlying security justification disappearing. Suppression requirement rises from 0.58 to 0.72 as enforcement becomes less self-sustaining and requires more active suppression of: imperial court attempts at political restoration, merchant-class demands for voice, daimyo assertion of autonomy. The accessibility_collapse (0.79) reflects that alternatives to the bakufu system were structurally closed off by samurai monopoly and imperial legitimacy lock-in. Resistance is moderate (0.43) not because the arrangement is widely accepted, but because the suppression mechanisms (ideological, military, institutional) successfully prevent organized resistance from coalescing.
 *
 * PERSPECTIVAL GAP:
 *   The shogun's seat and the emperor's seat compute radically differently. From the shogun's position, the arrangement is genuine coordination: the shogun operates authority derived from imperial mandate, enabling successive regimes without institutional collapse. The extraction appears incidental to the coordination. From the emperor's position (and from the daimyo and merchant seats), the arrangement is enforced suppression: imperial political autonomy is locked away, daimyo independence is constrained, merchant voice is excluded. The coordination frame masks structural extraction. From the samurai class seat, the arrangement is a beneficiary monopoly: governing authority is reserved to them and defended by imperial legitimacy and military capability. The engine should compute the shogun seat as experiencing lower extracted content (beneficiary directionality), the emperor seat as experiencing high extraction despite nominal legitimacy (identity-locked victim), and the samurai class as experiencing net benefit (organized, beneficiary directionality). The claim of Tangled Rope (coordination + asymmetric extraction) should emerge from the structural divergence between seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The emperor's directionality should be high (near 1.0, target/victim) despite nominal institutional power: identity_locked exit means the emperor cannot exit the role without dissolving the imperial institution; the constraint suppresses direct political action; directionality derives from trapped exit + suppression + victim declaration. The shogun's directionality is moderate-low (0.35-0.45, partial beneficiary): organized institutional power, constrained exit (dependent on legitimacy), but benefits from delegation structure and receives administrative authority. Samurai class directionality is low (0.25-0.35, beneficiary): organized power, identity-locked to governing role (the constraint defines their legitimacy), benefits from monopoly. Daimyo directionality is moderate-high (0.65-0.75, target): powerful locally but suppressed at the national level, constrained exit (subordination to bakufu is enforced), victims of autonomy extraction. Merchants are excluded (role: excluded, not a directionality position, but if seated would be high — powerful economically, trapped politically). No directionality override is needed; the structural derivation from beneficiary/victim + power + exit + suppression should produce the right distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy progression over the measurement interval. The founding_problem_status is 'contested' because historians disagree on whether the need for samurai military governance persists or disappeared centuries ago. At time_point 0, the coordination function was robust: the bifurcation genuinely solved the security problem. By time_point 30, the founding problem is dead (institutionalized samurai training replaced ad-hoc military necessity, daimyo peace held for centuries, no external threat required decentralized warrior authority). Yet the constraint persists: the extraction remains (commission-like samurai monopoly on authority), the suppression persists (daimyo subordination, merchant exclusion, imperial political suppression), and the theater_ratio rises (maintaining the fiction of delegation becomes more costly as the justification disappears). The disappearance_verdict is world_rearranges: if the constraint evaporated, merchant classes would assume governing roles within years, daimyo would reassert autonomy, the imperial court would resume political power, and the samurai monopoly would dissolve. The mandatrophy is resolved (or would be) by the type classification: if computed as snare (which the metrics support by rising extraction + rising theater), the engine flags the contradiction between the founding_problem_status='dead' and the constraint's persistence, triggering mandatrophy intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_authority_separability,
    'Is the bifurcation of legitimacy-granting (emperor) from authority-exercising (shogun) a structurally necessary accommodation, or a constructed suppression of imperial political capacity dressed as delegation?',
    'Counterfactual institutional analysis: did other civilizations achieve similar governance stability through different allocations of legitimacy and authority? Do crises within the bakufu system occur when the suppression of imperial authority becomes unstable?',
    'If bifurcation is necessary (coordination reading), the constraint is Tangled Rope with genuine coordination component. If bifurcation is a cover story for imperial suppression, the constraint is Snare with coordination framing. The reading-boundary itself depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_vs_authority_separability, conceptual, 'Whether the separation of legitimacy from authority is functionally necessary or performatively constructed.').

omega_variable(
    samurai_class_lock,
    'Is the samurai class''s monopoly on governing authority maintained by the delegation structure, or would the delegation structure require samurai monopoly regardless?',
    'Historical counterfactual: could the bakufu delegation have remained stable if merchant or other classes were gradually admitted to governing positions, or did the arrangement structurally depend on class exclusion?',
    'If class monopoly is intrinsic to delegation, samurai benefit is endemic. If class monopoly is contingent, the constraint could persist without samurai extraction if political voice were broadened — revealing the class exclusion as parasitic on the delegation structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(samurai_class_lock, empirical, 'Whether samurai class monopoly on authority is necessary to delegation or contingently coupled.').

omega_variable(
    imperial_suppression_mechanism,
    'Is imperial political suppression structural (the emperor cannot govern effectively without the samurai military apparatus) or internalized (the emperor accepts the ideology that ritual authority is sufficient and believes political involvement would be illegitimate)?',
    'Post-constraint analysis: when emperors in late bakufu periods attempted to increase political involvement, what resistance they met (institutional, military, ideological). Documentation of imperial court discourse: did suppression persist because of external barriers or because imperial members accepted the constraint as natural?',
    'If structural, the suppression can be lifted by reducing samurai military monopoly. If internalized, the suppression persists even after samurai authority fades — the constraint has been incorporated into imperial identity and institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_suppression_mechanism, empirical, 'Whether imperial political suppression is structural or internalized in imperial identity.').

omega_variable(
    kernel_reading_contest,
    'Is the bakufu delegation a genuine reading of a stable, continuous kernel (the mandate of heaven), or does it represent a reinterpretation of the kernel that diverges from the loyalist restoration reading such that no single framework can hold both?',
    'Hermeneutical analysis: do bakufu-era and loyalist-era commentaries agree on what the kernel (divine mandate, imperial authority) means, or do they propose fundamentally incompatible interpretations? Can both readings be held within Confucian tradition, or does accepting one require rejecting the other''s foundational claims?',
    'If coexistent readings of one kernel, the constraint is one reading among live alternatives. If logically foreclosing (the readings cannot coexist in one framework), this reading is the structure around which a particular institutional moment crystallized, and the loyalist reading represents a different institutional settlement of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the reading coexists with or forecloses the sibling loyalist restoration reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(impe_tr_t10, imperial_mandate__bakufu_delegation_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(impe_tr_t20, imperial_mandate__bakufu_delegation_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(impe_tr_t30, imperial_mandate__bakufu_delegation_reading, theater_ratio, 30, 0.51).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(impe_be_t10, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(impe_be_t20, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(impe_be_t30, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(impe_su_t10, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(impe_su_t20, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(impe_su_t30, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__bakufu_delegation_reading, 0.14).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% The imperial_mandate kernel decomposes into two constraint stories: bakufu_delegation_reading (this file) and loyalist_restoration_reading (sibling). Both stories instantiate the same kernel (divine authority, mandate of heaven) but propose different structural arrangements. The bakufu reading emphasizes bifurcated sovereignty and institutional continuity through delegation; the loyalist reading emphasizes unmediated imperial authority. They share the kernel and the measurement interval but have different beneficiaries, different epsilon values, and different temporal trajectories. The bakufu reading exhibits rising extraction over time (theater_ratio and suppression_requirement increase as the coordination function decays); the loyalist reading frames the bakufu arrangement as a suppression of legitimate imperial authority from the outset. The two readings coexist as live interpretations held by different factions; neither forecloses the other within Confucian tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
