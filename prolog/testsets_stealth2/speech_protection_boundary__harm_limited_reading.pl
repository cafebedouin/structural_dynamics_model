% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Harm-Limited Speech Protection Boundary (Dignity and Equality Condition)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   In this arrangement, speech protection is not a default status but a
 *   conditional one: expression retains protection only insofar as it causes
 *   no significant harm to dignity, equality, or freedom from harassment. A
 *   state apparatus of regulators drafting guidance, tribunals hearing
 *   complaints, and courts consolidating doctrine decides which statements
 *   clear the bar, and the unprotected set explicitly includes group
 *   vilification, harassment, and coded targeting. Members of targeted groups
 *   gain a recourse mechanism they could not build individually; speakers
 *   whose expression falls afoul of the standard bear uncertainty, defense
 *   costs, and penalties; and the administering state accumulates
 *   discretionary authority whose exercise depends on who holds it. The
 *   arrangement was built to remedy group-directed harm that narrower
 *   doctrine left unaddressed, and it has widened over its history from
 *   explicit vilification toward coded and indirect expression, with
 *   enforcement machinery growing alongside.
 *
 * KEY AGENTS:
 *   - - state_speech_regulators: Primary agenda-setter (institutional/arbitrage) - drafts the harm standard, sets enforcement priorities, collects adjudicative discretion
 *   - - constitutional_courts: Co-agenda-setter (institutional/constrained) - consolidates doctrine on which expressions fall outside protection
 *   - - members_of_targeted_groups: Primary beneficiary (moderate/constrained) - gains recourse against group-directed harm; secondarily exposed when their own speech is adjudicated
 *   - - antidiscrimination_advocacy_organizations: Secondary beneficiary (organized/mobile) - institutional standing scales with the standard's breadth
 *   - - dissenting_speakers: Primary payer (moderate/constrained) - carries uncertainty, self-censorship, and defense costs
 *   - - political_opposition_speakers: Payer (organized/constrained) - exposed to reframing of criticism as group-directed harm under hostile administrations
 *   - - satirical_and_academic_speakers: Payer (moderate/identity_locked) - vocational identity fused with boundary-testing expression
 *   - - platform_content_moderators: Dual-positioned payer/beneficiary (organized/arbitrage) - enforces the standard at scale, purchases safe harbor
 *   - - civil_liberties_organizations: Excluded voice (powerful/constrained) - objects from a premise the framework treats as bad faith
 *   - - international_human_rights_bodies: Analytical observer (institutional/analytical) - sees protective yields and suppression costs comparatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.66).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Harm-Limited Speech Protection Boundary (Dignity and Equality Condition)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '5a214192-2878-4ac9-b622-36f43fe7abdc').
narrative_ontology:cs_kernel_codification('5a214192-2878-4ac9-b622-36f43fe7abdc', fixed_text).
narrative_ontology:cs_authority_grounding('5a214192-2878-4ac9-b622-36f43fe7abdc', lineage).
narrative_ontology:cs_interpretation_layer_present('5a214192-2878-4ac9-b622-36f43fe7abdc').
narrative_ontology:cs_reading_relation('5a214192-2878-4ac9-b622-36f43fe7abdc', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('5a214192-2878-4ac9-b622-36f43fe7abdc', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('5a214192-2878-4ac9-b622-36f43fe7abdc', foundational, dignitary_harm_withdraws_protection).
narrative_ontology:cs_axiom_status(dignitary_harm_withdraws_protection, holdable).
narrative_ontology:cs_axiom_grounding('5a214192-2878-4ac9-b622-36f43fe7abdc', dignitary_harm_withdraws_protection, deontological).
narrative_ontology:cs_axiom('5a214192-2878-4ac9-b622-36f43fe7abdc', secondary, state_gatekeeping_legitimate_for_group_harm).
narrative_ontology:cs_axiom_status(state_gatekeeping_legitimate_for_group_harm, holdable).
narrative_ontology:cs_axiom_grounding('5a214192-2878-4ac9-b622-36f43fe7abdc', state_gatekeeping_legitimate_for_group_harm, instrumental).
narrative_ontology:cs_reference_frame('5a214192-2878-4ac9-b622-36f43fe7abdc', harm_conditional_protection_baseline).
narrative_ontology:cs_drift_state('5a214192-2878-4ac9-b622-36f43fe7abdc', contemporary_platform_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a214192-2878-4ac9-b622-36f43fe7abdc', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, members_of_targeted_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_speech_regulators).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, antidiscrimination_advocacy_organizations).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, dissenting_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, political_opposition_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, satirical_and_academic_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, platform_content_moderators).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, members_of_targeted_groups).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, platform_content_moderators).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignitarian_harm_conception).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, subordinating_speech_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the statutory harm definitions, issue interpretive guidance on what counts as significant harm to dignity, equality, or freedom from harassment, set enforcement priorities, and direct investigative and prosecutorial resources. Every widening of the unprotected set passes through their guidance documents. They collect expanded jurisdiction, staffing, and precedent-setting authority; their exit from any particular interpretation is rewriting the guidance, not leaving the system.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_speech_regulators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, state_speech_regulators, beneficiary).

% Decide case by case which expressions fall outside protection, building the doctrine that defines significant harm. Each ruling accumulates docket significance and doctrinal authority for the bench. They cannot decline the adjudicative role without abdicating their constitutional function, and their interpretations bind every other seat.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Gain a legal route against group-directed vilification and harassment that no individual avoidance strategy addresses: complaints, tribunal hearings, and remedies they could not obtain privately. The same standard also reaches their own sharp, retaliatory, or in-group speech, so they occasionally stand before the same tribunals as respondents. Leaving the jurisdiction or withdrawing from public life is the only way to step outside the arrangement, and both sacrifice home and voice.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, members_of_targeted_groups, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, members_of_targeted_groups, payer).

% Consult on standard design, intervene as interested parties in cases, staff and feed monitoring bodies, and receive funding streams tied to the standard's operation. Their institutional standing scales with the breadth of the unprotected set. They can redirect effort to adjacent causes if the standard narrows, so their attachment is organizational rather than existential.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, antidiscrimination_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Hold minority or contrarian views and publish them. Whether a given statement stays protected depends on an administrator's judgment about dignitary and equality harm, so they carry permanent uncertainty, self-censorship pressure, and occasional legal defense costs. Exit means abandoning public participation altogether; anonymous channels are fragile and increasingly scoped by platform rules.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, dissenting_speakers, payer,
    moderate, biographical, constrained, national).

% Opposition parties and movements criticize governing coalitions and their supporter bases. Critics of the standard argue that such criticism can be reframed as group-directed harm by a hostile administration. They possess organizational resources to litigate and campaign, but they cannot exit the jurisdiction's speech order without ceding political relevance, and each electoral turnover hands the standard to new administrators.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, political_opposition_speakers, payer,
    organized, generational, constrained, national).

% Produce transgressive satire, boundary-testing art, and controversial research. Their vocations consist precisely in saying things that alarm; an investigation or prosecution under the harm standard strikes at the core of professional identity rather than at a peripheral activity. Stopping the provocation would end the vocation, so exit from the standard's reach is equivalent to career death.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, satirical_and_academic_speakers, payer,
    moderate, biographical, identity_locked, national).

% Operationalize state harm definitions at scale: build classifiers, remove content, answer takedown demands, and absorb over-removal and under-removal failures. Compliance purchases safe-harbor treatment and regulatory goodwill, while the costs land on their users and their engineering budgets. Operating across many jurisdictions, they can geofence obligations and shop for lenient regimes, which softens their exposure relative to domestic speakers.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, platform_content_moderators, payer,
    organized, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, platform_content_moderators, beneficiary).

% Contend that a vagueness-prone harm standard is a standing instrument for suppressing dissent regardless of the intentions of current administrators. They litigate, publish, and campaign, but their core premise - that dignitary offense is not a ground for withdrawing protection - is treated within the prevailing framework as bad faith or fringe, so their objection enters proceedings only as a losing argument rather than as a competing design.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_organizations, excluded,
    powerful, generational, constrained, national).

% Review state compliance with free-expression and anti-discrimination treaties, issue findings and recommendations, and compile comparative data across jurisdictions. They hold no enforcement power domestically and occupy an analytical seat over the whole arrangement, seeing both the protective yields and the suppression costs that domestic participants experience piecemeal.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, state_speech_regulators).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an enforceable baseline of communicative conduct so that members of vulnerable groups can enter public discourse without individually bearing unbounded dignitary and safety costs; converts a dispersed, hard-to-deter pattern of group-directed abuse into a centrally actionable category.
% TRANSFER_FUNCTION: Moves adjudicative authority over expressive conduct from speakers to state institutions; moves expressive risk from targeted group members to speakers whose statements fall outside the protected set; moves public enforcement resources toward policing communicative harm.
% ABSENT_VOICES: Civil-liberties and absolutist-leaning speakers would object that the standard's vagueness makes it a durable suppression instrument, but their premise is structurally outside the framework's terms. Future administrations that will inherit the gatekeeping standard are also absent - the standard's designers do not sit in the seat of its eventual holders, and the abuse risk lands on that empty chair.
% DISAPPEARANCE_RATIONALE: If the harm-conditioned boundary vanished overnight, currently restricted categories (group vilification, harassment, coded targeting) would revert to protected speech; targeted-group members would lose their recourse mechanism; platforms would re-align moderation policies within weeks; and the state would lose an established instrument of communicative governance along with the institutional complexes built around it.
% FOUNDING_PROBLEM: Group-directed vilification, sustained harassment, and incitement were producing measurable exclusion, fear, and violence against identifiable groups, while the prevailing narrow standard left those harms without legal remedy.
% FOUNDING_PROBLEM_CORROBORATION: National statistics agencies' hate-crime series and victimization surveys - produced outside the benefiting parties - document that group-directed harm persists at scale. Historical records from before the standard's adoption corroborate the original problem. Civil-liberties organizations corroborate the problem's existence while disputing the proportionality of the remedy, which is contestation of means, not denial of the founding problem.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.62 at interval end) because the standard delivers real protection while transferring open-ended adjudicative discretion to the state: the same vagueness that lets tribunals reach coded targeting lets a future administration reach ordinary dissent. Suppression (0.66) reflects the machinery the arrangement requires - complaint systems, specialized tribunals, monitoring bodies, platform mandates - because speakers' default preference runs to unregulated expression and the boundary must be actively policed to hold. Theater (0.28) is moderate-low: most enforcement resolves concrete complaints, though a growing share consists of high-profile prosecutions and guidance documents whose function is signaling resolve. Accessibility collapse (0.5): narrower statutory routes and other jurisdictions' boundary designs remain visible and partly usable, so alternatives dim but do not vanish. Resistance (0.68): sustained litigation, opposition campaigning, and civil-liberties contestation meet the standard continuously. The measurement series run on one shared time grid (points 0-50 at decade steps) so every tracked metric is authored at every examined point; final values equal the base_properties scalars. Suppression is authored as a raw structural property and is not scaled by directionality or scope; only extractiveness is scaled downstream by the engine.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats experience the arrangement as rights implementation: guidance, dockets, and remedies executing an equality commitment. The payer seats experience the same structure as contingent permission - their speech stays protected only until an administrator judges otherwise, and the judge changes with every election. Beneficiary seats experience it as the floor beneath public participation. The engine computes these divergent per-seat types from the structural data; the divergence between the regulator's administrative view and the dissenter's contingency view is the arrangement's central perspectival fact.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulators and courts sit near the beneficiary end: they collect jurisdiction, staffing, and precedent authority, and their exit is reinterpretation rather than departure. Members of targeted groups derive near-full-beneficiary directionality from their declared role, but their true position sits slightly toward the target end because the same standard adjudicates their own sharp or retaliatory speech - this dual exposure is declared via secondary_role. Antidiscrimination organizations are straightforward beneficiaries with mobile exit. Dissenting and opposition speakers sit near the target end: they bear the transfer and their exit options are thin. Satirical and academic speakers are targets with identity-locked exit - their vocation is the provocation, so the standard reaches their professional core. Platform moderators are dual-positioned: primary payer (compliance costs, over-removal liability) but purchasing safe-harbor legitimacy, pulling their true position toward symmetric - again declared via secondary_role. No directionality overrides are authored: the derivation from declared roles, power, and exit captures each seat adequately, and the available override granularity (per power atom) would misassign same-power seats with opposed positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - unremedied group-directed harm - is live and externally corroborated, so this is not yet a mandate outliving its function; mandatrophy is not resolved. The hybrid classification matters because both mislabels are live temptations: reading the arrangement as pure coordination erases the discretionary power the state accumulates and the dissenters who fund that accumulation with their speech; reading it as pure extraction erases the recourse mechanism targeted-group members actually use and would lose. The tangled-rope framing keeps both facts load-bearing. The rising extractiveness series is the drift signal to watch: if enforcement decouples further from declared criteria (see the capture-risk omega), the hybrid degrades toward pure extraction with the coordination story as cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_dispute,
    'Which reading of the speech protection boundary should govern: this harm-conditioned reading, the near-absolute reading confined to imminent lawless action, or case-by-case balancing?',
    'Constitutional amendment, sustained doctrinal reversal by apex courts, or cross-jurisdictional convergence after comparative outcome study.',
    'Adoption of the near-absolute sibling dissolves this reading''s unprotected set back into protected speech and removes the state gatekeeper; adoption of the balancing sibling replaces the categorical condition with open-ended weighing and relocates discretion from rule to adjudicator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_dispute, conceptual, 'This constraint is one reading of the speech_protection_boundary kernel; sibling readings instantiate different boundary rules with different victim sets.').

omega_variable(
    gatekeeper_capture_risk,
    'Does enforcement under the significant-harm standard track the declared dignitary and equality criteria, or does it track the administering government''s political interests?',
    'Cross-administration audit of enforcement patterns: compare case selection, target profiles, and outcomes across successive governments of different composition.',
    'If enforcement tracks political interest, the arrangement''s effective burden concentrates on opposition seats and the classification shifts toward pure extraction with the protective story as cover; if it tracks declared criteria, the hybrid coordination-plus-discretion reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_capture_risk, empirical, 'Whether the state gatekeeper function is captured or criterion-bound.').

omega_variable(
    dog_whistle_determinacy,
    'Can coded dog-whistle expression be identified by criteria stable enough to bound administrator discretion, or is the category inherently interpreter-relative?',
    'Inter-rater reliability studies of dog-whistle classifications across raters of differing political alignment, plus appellate reversal-rate analysis on coding-based cases.',
    'If the category is interpreter-relative, the unprotected set is administrator-relative and the discretion component of the burden on speakers is effectively unbounded; if determinate, the boundary constrains the gatekeeper as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dog_whistle_determinacy, conceptual, 'Determinacy of the coded-speech category that widens the unprotected set.').

omega_variable(
    chilling_effect_magnitude,
    'How much lawful speech is deterred beyond the formally restricted categories by the mere existence of the harm standard?',
    'Speaker surveys measuring self-censorship, natural experiments around standard adoption or narrowing, and comparison of publication rates in affected domains before and after enforcement intensification.',
    'Higher chilling raises the effective burden on every payer seat above the formal measure and strengthens the case that the standard''s total cost exceeds its restricted-set footprint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Off-book deterrence carried by speakers who never appear in enforcement statistics.').

omega_variable(
    narrow_means_separability,
    'Could targeted harassment, incitement, and threat statutes achieve most of the protective yield without the general harm-conditioned boundary?',
    'Comparative outcomes across jurisdictions that protect aggressively but regulate narrowly versus those with broad harm-conditioned standards, controlling for baseline harm rates.',
    'If the functions are separable, the excess breadth is discretionary power riding on a genuine protective core; if inseparable, part of the measured burden is the irreducible price of the protection itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_means_separability, empirical, 'Separability of the protective function from the general boundary-widening mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spbl_harm_limited_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t0, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t10, speech_protection_boundary__harm_limited_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t10, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t20, speech_protection_boundary__harm_limited_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t20, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t30, speech_protection_boundary__harm_limited_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t30, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t40, speech_protection_boundary__harm_limited_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t40, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t50, speech_protection_boundary__harm_limited_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(spbl_harm_limited_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t0, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t10, speech_protection_boundary__harm_limited_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t10, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t20, speech_protection_boundary__harm_limited_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t20, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t30, speech_protection_boundary__harm_limited_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t30, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t40, speech_protection_boundary__harm_limited_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t40, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t50, speech_protection_boundary__harm_limited_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(spbl_harm_limited_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t0, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t10, speech_protection_boundary__harm_limited_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t10, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t20, speech_protection_boundary__harm_limited_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t20, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t30, speech_protection_boundary__harm_limited_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t30, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t40, speech_protection_boundary__harm_limited_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t40, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t50, speech_protection_boundary__harm_limited_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% The colloquial 'free speech debate' decomposes into three structurally distinct boundary rules, not one contestable constraint: each reading fixes a different protected set, a different victim population, and a different enforcement shape, so each carries its own stable epsilon. Family links run absolutist_reading -> harm_limited_reading -> balancing_reading: the absolutist rule is the upstream reference whose narrowness motivated the harm-conditioned correction, and the harm-conditioned rule in turn reshapes the environment in which balancing operates. This file is the middle member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
