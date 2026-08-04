% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence — Contraction Reading
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the contraction reading of the
 *   honor_violence_legitimacy kernel: dueling became structurally unthinkable
 *   not because external costs made it impractical (the drop reading), nor
 *   because both mechanisms operated simultaneously (the composite reading),
 *   but because honor itself was redefined to exclude violence as a
 *   legitimate response. The constraint is the commitment system of modern
 *   honor that renders violent vindication conceptually impossible. It
 *   operates as a tangled rope: it genuinely coordinates social order by
 *   removing lethal private violence from status competition, while
 *   asymmetrically extracting from aristocratic identity structures and
 *   marginalized men who lack access to the new honor economy.
 *
 * KEY AGENTS:
 *   - centralizing_state: Primary agenda-setter (institutional/analytical) — legislates against dueling and monopolizes legitimate violence.
 *   - commercial_bourgeoisie: Primary beneficiary (powerful/mobile) — gains commercial predictability and gentility without martial risk.
 *   - traditional_aristocracy: Primary payer (powerful/identity_locked) — loses constitutive status mechanism as violent honor is delegitimized.
 *   - marginalized_men: Secondary payer (powerless/trapped) — excluded from both old violent honor and new property-based respectability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.62).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence — Contraction Reading").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'abcc22bd-81aa-404c-994e-82a9f2fdb810').
narrative_ontology:cs_kernel_codification('abcc22bd-81aa-404c-994e-82a9f2fdb810', distributed).
narrative_ontology:cs_authority_grounding('abcc22bd-81aa-404c-994e-82a9f2fdb810', practice).
narrative_ontology:cs_interpretation_layer_present('abcc22bd-81aa-404c-994e-82a9f2fdb810').
narrative_ontology:cs_reading_relation('abcc22bd-81aa-404c-994e-82a9f2fdb810', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('abcc22bd-81aa-404c-994e-82a9f2fdb810', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('abcc22bd-81aa-404c-994e-82a9f2fdb810', foundational, honor_excludes_violent_vindication).
narrative_ontology:cs_axiom_status(honor_excludes_violent_vindication, holdable).
narrative_ontology:cs_axiom_grounding('abcc22bd-81aa-404c-994e-82a9f2fdb810', honor_excludes_violent_vindication, conventional).
narrative_ontology:cs_axiom('abcc22bd-81aa-404c-994e-82a9f2fdb810', secondary, moral_character_supersedes_blood_vengeance).
narrative_ontology:cs_axiom_status(moral_character_supersedes_blood_vengeance, holdable).
narrative_ontology:cs_axiom_grounding('abcc22bd-81aa-404c-994e-82a9f2fdb810', moral_character_supersedes_blood_vengeance, deontological).
narrative_ontology:cs_reference_frame('abcc22bd-81aa-404c-994e-82a9f2fdb810', honor_as_moral_integrity).
narrative_ontology:cs_drift_state('abcc22bd-81aa-404c-994e-82a9f2fdb810', early_modern_dueling_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('abcc22bd-81aa-404c-994e-82a9f2fdb810', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, centralizing_state).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, commercial_bourgeoisie).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, traditional_aristocracy).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, marginalized_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates against dueling, prosecutes participants, and monopolizes legitimate violence. Benefits from reduced private lethal force and enhanced sovereign authority over elite male conduct.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, centralizing_state, agenda_setter,
    institutional, generational, analytical, national).

% Gains predictable commercial relations where disputes do not escalate to lethal encounters. Claims gentlemanly status through property and respectability rather than martial prowess or willingness to duel.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, commercial_bourgeoisie, beneficiary,
    powerful, biographical, mobile, national).

% Bears the cultural and status cost of a delegitimized violent honor repertoire. The duel was a core mechanism of aristocratic distinction and boundary maintenance; its exclusion renders their honor culture archaic and morally suspect.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, traditional_aristocracy, payer,
    powerful, generational, identity_locked, national).

% Lack the social and economic capital for the new moralized honor based on respectability and property. Cannot duel (it is illegal and unthinkable) and lack alternative status mechanisms, leaving them without legitimate recourse for grievances.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, marginalized_men, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, centralizing_state).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolving status grievances and reputation disputes without lethal private violence; enabling predictable commercial, political, and social interaction by removing the threat of honor-based killing from everyday elite and middle-class life.
% TRANSFER_FUNCTION: Moves the authority to define, adjudicate, and enforce honor from aristocratic peer recognition and self-help to state-backed legal frameworks and bourgeois moral norms. Moves status security from martial prowess and blood vindication to property ownership and moral respectability.
% ABSENT_VOICES: Traditional aristocrats whose honor culture was constituted by violent vindication; rural and peripheral men for whom the new moralized honor remained economically and socially inaccessible; women, who were largely excluded from the masculine honor discourse that redefined legitimate virtue but bore the familial costs of both dueling and its legal suppression.
% DISAPPEARANCE_RATIONALE: If honor suddenly re-admitted violence as a legitimate response, private dueling would resurge among those with status grievances, the state's monopoly on legitimate force would fracture, commercial society would face unpredictable lethal risk in contractual and personal disputes, and aristocratic status mechanisms would reactivate — the social order would reorganize around violent masculine self-help.
% FOUNDING_PROBLEM: Lethal private violence as a routine mechanism for elite status and dispute resolution destabilized early modern states, decimated aristocratic male populations, and disrupted commerce.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and historical sociologists attest the quantitative decline of aristocratic lethal violence in Western Europe. However, the state and commercial elites who benefited most from the redefinition also narrate its necessity. Independent anthropological studies of honor cultures where violence remained structurally legitimate (e.g., Mediterranean, Middle Eastern, Central Asian contexts) provide external corroboration that the founding problem was culturally and politically specific rather than universal.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.25 to 0.68 over the interval as the new honor code consolidates and the aristocratic alternative is systematically delegitimized. Suppression rises from 0.30 to 0.78 as state legal penalties harden and social ostracism intensifies against dueling. Theater ratio is moderate (0.40 at endpoint) because honor performances persist but are redirected toward moral display, legal process, and commercial reputation rather than lethal combat. Accessibility collapse is high (0.82) because once the conceptual redefinition is accepted, violent honor responses become literally unthinkable within the legitimate vocabulary. Resistance is moderate (0.45) because aristocratic pushback was significant but gradually subdued through incorporation, ridicule, and legal sanction. The measurement series share one aligned time grid representing the longue durée transition from early modern dueling culture to nineteenth-century bourgeois honor.
 *
 * PERSPECTIVAL GAP:
 *   The state seat computes this constraint as necessary civilizing coordination that monopolizes violence and protects commerce. The aristocratic seat computes it as cultural dispossession — the extraction of their constitutive status language. The marginalized_men seat computes it as double exclusion, lacking the resources for new honor and the legitimacy for old honor. The engine produces these divergent seat classifications from the same structural data because beneficiary/victim declarations and exit modulation remap identical power atoms to very different directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The centralizing_state and commercial_bourgeoisie are structural beneficiaries: the state captures sovereignty and enforcement authority, the bourgeoisie captures commercial predictability and status mobility. Their directionality sits near the beneficiary end. Traditional_aristocracy and marginalized_men are structural targets: the aristocracy loses a constitutive identity practice (identity_locked exit amplifies their effective extraction), while marginalized men are trapped between illegitimate violence and inaccessible respectability. The same-level lateral divergence between aristocracy and bourgeoisie — both powerful in nominal terms — is driven entirely by exit options: the bourgeoisie is mobile across honor regimes, the aristocracy is identity_locked to the old code.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this constraint as either pure coordination (rope) or pure extraction (snare). The coordination function is real and ongoing: modern society continues to require non-violent status mechanisms. However, the constraint is not merely a rope because it asymmetrically transfers honor-authority from aristocratic martial identity to state/bourgeois structures. It is not a piton because the coordination function has not atrophied — the constraint persists because it continues to solve the problem of regulating status without lethal violence, not because of theatrical inertia. The founding problem is contested rather than dead, which further insulates against mandatrophy misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_kernel_constructed_vs_discovered,
    'Is the exclusion of violence from honor a discovered moral truth inherent to the kernel, or a contingent social construction imposed by emerging state and commercial interests?',
    'Comparative historical analysis of honor concepts across cultures and periods; if non-violent honor is culturally specific to state-consolidating commercial societies, the construction thesis is supported.',
    'If constructed, contraction_reading is a legitimization narrative for state and bourgeois interests; if discovered, it is a genuine moral clarification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_kernel_constructed_vs_discovered, conceptual, 'Whether honor''s exclusion of violence is natural or constructed').

omega_variable(
    structural_unthinkability_vs_practical_rarity,
    'Did dueling become conceptually impossible within honor (contraction), or did it remain conceptually legitimate but become practically unsustainable (drop)?',
    'Discourse analysis of honor rhetoric in personal letters, legal defenses, and literature from the transition period: if dueling disappears from the honorable imagination entirely, contraction is supported; if it persists as an aspirational ideal thwarted by costs, drop is supported.',
    'Resolves the reading rivalry and determines whether the constraint is primarily cognitive and normative (contraction) or economic and coercive (drop).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_unthinkability_vs_practical_rarity, empirical, 'Empirical test between conceptual and cost-based decline of dueling').

omega_variable(
    suppression_internalized_or_structural,
    'Is the constraint''s force primarily internalized shame (the honorable man cannot conceive of dueling) or structural enforcement (legal penalties, social ostracism, professional ruin)?',
    'Analysis of transitional dueling cases and memoirs: if men refrained despite willingness to risk legal penalty, internalization is high; if cessation tracks enforcement intensity and legal precedent, suppression is structural.',
    'If internalized, effective extraction is higher than structural measures suggest and the constraint operates as identity-locked cognitive capture; if structural, the constraint is more brittle and would weaken under enforcement decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_or_structural, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__contraction_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__contraction_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(hono_tr_t90, honor_violence_legitimacy__contraction_reading, theater_ratio, 90, 0.35).
narrative_ontology:measurement(hono_tr_t120, honor_violence_legitimacy__contraction_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement(hono_tr_t150, honor_violence_legitimacy__contraction_reading, theater_ratio, 150, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__contraction_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__contraction_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(hono_be_t90, honor_violence_legitimacy__contraction_reading, base_extractiveness, 90, 0.55).
narrative_ontology:measurement(hono_be_t120, honor_violence_legitimacy__contraction_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(hono_be_t150, honor_violence_legitimacy__contraction_reading, base_extractiveness, 150, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__contraction_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__contraction_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(hono_su_t90, honor_violence_legitimacy__contraction_reading, suppression_requirement, 90, 0.65).
narrative_ontology:measurement(hono_su_t120, honor_violence_legitimacy__contraction_reading, suppression_requirement, 120, 0.72).
narrative_ontology:measurement(hono_su_t150, honor_violence_legitimacy__contraction_reading, suppression_requirement, 150, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
