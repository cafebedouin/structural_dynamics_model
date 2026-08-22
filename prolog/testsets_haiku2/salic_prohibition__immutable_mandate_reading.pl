% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Prohibition: Immutable Agnatic Succession Mandate
 *   domain: constitutional/dynastic
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   Salic prohibition. The immutable-mandate reading treats Salic Law as
 *   irrevocable natural/divine law embedded in dynastic constitutional order
 *   — a claim that female succession is categorically impossible, not merely
 *   prohibited by current positive law but violative of the order of nature
 *   and the will of God. Under this reading, female heirs are excluded not by
 *   policy choice but by ontological fact; the prohibition is not revocable
 *   by sovereign authority because no sovereign has the authority to rewrite
 *   natural law. This reading justifies preventive war against female
 *   claimants and delegitimizes any challenge to agnatic priority as heresy
 *   against nature itself. The immutable-mandate reading coexists with two
 *   sibling readings: the sovereign-override reading (Salic Law is revocable
 *   positive law subject to legislative authority) and the cognatic-reversion
 *   reading (Salic Law is a Frankish anachronism inapplicable to non-Frankish
 *   territories and modern succession systems). The constraint as authored
 *   here captures only the immutable-mandate reading — treating other
 *   readings as separate constraints linked through the kernel.
 *
 * KEY AGENTS:
 *   - Agnatic male heirs (powerful beneficiaries): guaranteed succession by mandate; collects succession rents (crown, lands, authority)
 *   - Female heirs (powerful targets): categorically excluded from succession despite potential fitness; trapped by legal disability on their entire sex
 *   - Patrimonial hierarchy defenders (institutional agenda-setter): clergy, legal interpreters, noble councils maintaining the mandate as divinely ordained; power derives from authoritative interpretation
 *   - Cognatic succession advocates (organized payers): argue the prohibition is anachronistic; excluded from authoritative voice on the mandate's legitimacy
 *   - Royal clergy (institutional agenda-setter): provide doctrinal authority for divine will and natural order; interpret Scripture and canon law to vindicate agnatic priority
 *   - Rival dynastic powers (powerful observers): recognize or reject female claimants as strategic advantage dictates; their position determines legitimacy in disputed successions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.78).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.89).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Prohibition: Immutable Agnatic Succession Mandate").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/dynastic").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).
domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '97c38645-1144-4f85-884d-f7ceffa5e746').
narrative_ontology:cs_kernel_codification('97c38645-1144-4f85-884d-f7ceffa5e746', fixed_text).
narrative_ontology:cs_authority_grounding('97c38645-1144-4f85-884d-f7ceffa5e746', lineage).
narrative_ontology:cs_interpretation_layer_present('97c38645-1144-4f85-884d-f7ceffa5e746').
narrative_ontology:cs_reading_relation('97c38645-1144-4f85-884d-f7ceffa5e746', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('97c38645-1144-4f85-884d-f7ceffa5e746', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('97c38645-1144-4f85-884d-f7ceffa5e746', foundational, agnatic_succession_divinely_mandated).
narrative_ontology:cs_axiom_status(agnatic_succession_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('97c38645-1144-4f85-884d-f7ceffa5e746', agnatic_succession_divinely_mandated, theological).
narrative_ontology:cs_axiom('97c38645-1144-4f85-884d-f7ceffa5e746', foundational, female_succession_categorically_impermissible).
narrative_ontology:cs_axiom_status(female_succession_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('97c38645-1144-4f85-884d-f7ceffa5e746', female_succession_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('97c38645-1144-4f85-884d-f7ceffa5e746', agnatic_natural_order).
narrative_ontology:cs_drift_state('97c38645-1144-4f85-884d-f7ceffa5e746', late_medieval_challenge_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97c38645-1144-4f85-884d-f7ceffa5e746', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, patrimonial_hierarchy_defenders).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_succession_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, general_population).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_natural_order).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, divine_patriarchal_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Guaranteed succession regardless of capability or primacy of female siblings. The constraint vests succession in the male line exclusively, ensuring that legitimate male claimants cannot be displaced by female heirs. Their power derives from the mandate itself: they benefit from legal protection of their succession rights and from the doctrine that enforces exclusion of female competitors.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs, beneficiary,
    powerful, generational, mobile, national).

% Excluded from succession regardless of birth order or fitness to rule. The constraint operates as a blanket legal disability on female persons from the royal house itself — not deprivation of ordinary property rights but categorical preclusion from the highest office and the rents attached. No appeal, no exception, no countervailing claim registers within the agnatic framework. Female heirs can influence advisors or exercise power through sons but cannot hold the crown in their own right.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerful, generational, trapped, national).

% Maintains and enforces the Salic prohibition through canonical authority, legal opinion, and succession adjudication. The clergy, noble councils, and royal advisors who interpret and defend the mandate as divinely ordained and legally unrevocable. Their power derives from their role as authoritative interpreters: they determine what the mandate requires and adjudicate succession disputes in light of it.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, patrimonial_hierarchy_defenders, agenda_setter,
    institutional, generational, mobile, national).

% Argue that the prohibition is a Frankish particular anachronism, not binding on non-Frankish dynasties or on contemporary succession arrangements, and that female succession is a legitimate dynastic choice. They are excluded from the authoritative interpretation of the mandate — their reading of Salic Law as revocable or inapplicable is treated as heretical or illegitimate. Their constraint is that the agnatic mandate forecloses this alternative reading from holding political force.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_succession_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, cognatic_succession_advocates, excluded).

% Potential female heirs from cadet branches or marriages to foreign houses who might press succession claims if the prohibition were lifted. They are excluded from the conversation about succession rights by the same mandate that bars female succession — their exclusion is structural to the constraint's enforcement.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_cadet_claimants, excluded,
    moderate, biographical, trapped, national).

% Provides doctrinal and spiritual authority for the agnatic mandate, citing divine will and natural order. Interprets Scripture and canon law to vindicate the prohibition. Their power derives from their role as authoritative interpreters of divine and natural law — they can legitimate or delegitimate the constraint through doctrinal pronouncement.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, royal_clergy, agenda_setter,
    institutional, generational, mobile, national).

% Other kingdoms and powers that may or may not adopt the Salic prohibition. Their position on the constraint determines succession disputes in border territories and legitimacy contests. They observe the constraint's operation but are not bound by it — they can recognize or reject female claimants to the throne as strategic advantage dictates.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, rival_dynastic_powers, observer,
    powerful, generational, mobile, national).

% Benefits from dynastic stability secured by clear agnatic succession rules, at the cost of accepting female exclusion as natural/divine law. They have no formal say in succession disputes but bear the consequences of succession conflicts. The constraint is presented as protecting them from disorder; contestation of the prohibition is framed as threatening stability.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, general_population, beneficiary,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, contestation-resistant rule for dynastic succession that precludes female claimants from creating competing legitimacy lines. Solves the coordination problem of succession disputes by providing an unambiguous criterion that forecloses a category of potential claimants.
% TRANSFER_FUNCTION: Transfers succession rights, the rents and authority attached to the crown, and political legitimacy from the female line to the male line exclusively. Moves dynastic power and the resources of the realm from potential female rulers to agnatic heirs.
% ABSENT_VOICES: Female potential successors are excluded from argument about whether the rule should apply to them. Advocates for cognatic succession or female succession rights are excluded from authoritative interpretation of the mandate. Cadet claimants and foreign-married heirs who might benefit from alternative succession rules are not heard in the adjudication of the prohibition's legitimacy.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition disappeared overnight, multiple female claimants and cadet lines would immediately press succession claims; succession wars would likely follow; the entire dynastic legitimacy structure would be contested and renegotiated. The constraint's removal would trigger precisely the succession chaos it was designed to prevent — its absence is not stability, it is disorder that would force institutional reorganization.
% FOUNDING_PROBLEM: Early Frankish kingdoms needed a rule to prevent female succession claims from splintering the realm and creating competing legitimacy centers that rival powers could exploit. The Salic prohibition was encoded as immutable law to end the succession contests that had plagued the succession after Charlemagne.
% FOUNDING_PROBLEM_CORROBORATION: Medieval chroniclers and clergy who defend the Salic prohibition attest the founding problem is live: female succession would create disorder and foreign interference. Modern historians and legal scholars outside the benefiting parties (male heirs, patrimonial defenders) attest that alternative succession systems (including female succession) operated stably in contemporary non-Frankish dynasties, and that the founding problem was solved not by the prohibition's immutability but by clear succession rules that happened to be agnatic. The corroboration is mixed: the problem the prohibition was designed to solve is attested by its defenders; the claim that it is the ONLY solution is not corroborated outside the benefiting seat.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is authored as CLAIMED mountain (emerges_naturally: true, immutable mandate reading) while the metrics describe substantially extractive, actively enforced operation: extractiveness 0.78 (high rents to agnatic heirs), suppression 0.89 (powerful machinery to exclude female claimants and delegitimize alternatives), theater_ratio 0.41 (moderate performative component — the doctrine of divine order is invoked as justification for enforcement, suggesting some portion of the constraint's persistence relies on narrative rather than pure coercive power). The measurement series shows acceleration: extractiveness rises from 0.62 to 0.78 over the interval (rents concentrating, agnatic priority entrenched), while theater plateaus at 0.41 (the doctrinal justification is stable, not increasing). This pattern — rising extraction with stable theater — is consistent with a constraint whose coercive enforcement is being hardened while its narrative legitimation is already fixed. The accessibility_collapse (0.92) is high because female alternatives are nearly completely foreclosed once the agnatic mandate is accepted as natural law — no exit, no legal challenge succeeds, no alternative framing registers. The resistance (0.72) is substantial because female heirs and cognatic advocates mount real challenges to the immutability claim, though they are excluded from decisive voice. The claim/metric divergence is not a mistake; it is the false-summit candidate: a constraint claiming to be natural law while exhibiting the behavioral signature of constructed extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats will compute different types from identical structural data. Agnatic heirs and their defenders experience the constraint as natural law, coordination, or justified hierarchy. Female heirs and advocates experience it as extraction. The engine computes this divergence per-seat from power, exit, beneficiary/victim declarations — the authored claim does not determine the per-seat type.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male heirs benefit structurally: the mandate guarantees them succession regardless of female siblings' birth order or capability, moving the succession rents (crown, authority, lands) to the male line exclusively. Their exit from the constraint is not costly — they can abandon it only by ceding their rents, so they are not trapped. Their directionality is near 0.0 (full beneficiary). Female heirs bear the costs: they are excluded from the highest office not by policy preference but by categorical legal disability. They cannot exit by exiting the kingdom (the disability follows them as a female person); they are trapped by identity (female) + political role (heir). Their directionality is near 1.0 (full target). Patrimonial defenders benefit from authority and maintain the mandate — their directionality is near 0.0-0.3 (beneficiary-adjacent: they collect authority but frame it as duty). Cognatic advocates and female cadet claimants are excluded from the authoritative voice on the mandate's legitimacy — their directionality is near 1.0 (they bear the constraint as exclusion). The beneficiary/victim split is sharp: agnatic males vs. female heirs and cognatic advocates.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was succession disorder in early Frankish kingdoms. The immutable-mandate reading claims the problem is LIVE and eternal: female succession would create chaos perpetually. But historical evidence from contemporary non-Frankish dynasties (cognatic systems operating stably, female succession achieving equal succession clarity in some contexts) suggests the founding problem was solved by CLEAR SUCCESSION RULES, not by agnatic-only rules. The immutable-mandate reading conflates 'we chose agnatic rules' with 'only agnatic rules work' — a claim not corroborated outside the benefiting parties. Theater is rising (0.18 to 0.41 over the interval): the doctrinal apparatus is increasingly devoted to defending the immutability claim against cognatic alternatives rather than to managing actual succession disputes. This suggests the constraint is shifting from functional (preventing chaos) to theatrical (vindicating a narrative). The false-summit gate should fire: beneficiaries + natural-law claim + rising theater + alternative systems operating stably elsewhere = plausible false summit. An omega documents the irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_mandate,
    'Is the Salic prohibition a true natural law (the objective order of human generation and inheritance reflecting divine design) or a constructed legal rule that benefits male heirs and presents itself as natural/divine?',
    'Comparative historical analysis: if Salic agnatic succession is universal across human societies and across non-Frankish dynasties in the same epoch, it is plausibly natural law. If it is localized to Frankish tradition and contested or absent elsewhere, it is structurally constructed. If female succession operates stably in contemporary non-Frankish systems, the ''necessity'' claim fails.',
    'If natural law, the constraint is Mountain throughout. If constructed, the constraint is Tangled Rope or Snare: extraction (male succession rents) coordinated (stable succession rule) but asymmetrically enforced. The false-summit gate fires when beneficiaries + natural-law claim diverge from comparative evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_mandate, empirical, 'Whether the prohibition reflects the order of nature or human political choice.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.89) structural — external enforcement machinery that would cease if the law were revoked — or internalized — female heirs and cognatic advocates have been socialized to treat exclusion as inevitable and legitimate such that suppression persists even after the legal rule is removed?',
    'Post-revolution empirical data: jurisdictions that repeal the Salic prohibition and observe whether female claimants immediately press succession claims (structural suppression) or whether internalized acceptance of male-only succession persists for generations (internalized suppression). Historical evidence from other jurisdictions that successfully adopted female succession without prior legal foundation.',
    'If structural, revocation of the prohibition ends suppression. If internalized, the constraint''s effective suppression is higher than the legal measure suggests — removal of the formal rule does not automatically empower female heirs. The distinction affects remedial design: structural suppression calls for rule revision; internalized suppression requires societal re-education.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is external enforcement or internalized acceptance.').

omega_variable(
    cognatic_vs_agnatic_sufficiency,
    'Is agnatic-only succession necessary to prevent the succession disorders the prohibition was designed to prevent, or do stable cognatic systems (female succession permitted or preferred in some circumstances) achieve equal succession clarity without the extraction cost to female heirs?',
    'Comparative institutional analysis of cognatic and agnatic monarchies in the same epoch: succession stability metrics (disputes per century, contested successions, wars of succession), longevity of dynasty, legitimacy acceptance. If cognatic systems show equal or superior stability outcomes, the necessity claim for agnatic-only succession fails.',
    'If agnatic-only is necessary, the extraction is the price of coordination (Rope-compatible framing). If cognatic alternatives achieve equal stability, the extraction is pure rent-seeking dressed in stability language (Snare-supporting analysis). This drives mandatrophy and false-summit assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognatic_vs_agnatic_sufficiency, empirical, 'Whether agnatic succession is the only stable system or one option among comparable alternatives.').

omega_variable(
    immutable_mandate_reading_credibility,
    'This constraint instantiates the ''immutable mandate reading'' of the Salic kernel — the reading that treats the prohibition as irrevocable divine/natural law embedded in the constitution. But is this reading itself credible within the tradition, or has the interpreting authority (clergy, nobility) always understood the prohibition as revocable positive law subject to sovereign override?',
    'Doctrinal textual analysis: examine canonical sources, theological commentaries, and noble councils'' treatment of the prohibition. If all authoritative sources treat it as revocable, the immutable reading is post-hoc theorization, not a live tradition. If some sources support immutability and others treat it as revocable, the immutability is contested (supporting the kernel contest framing). If dominant sources consistently invoke immutability, the reading is credible within its own tradition.',
    'If the immutable reading is a fabrication or minority position, this constraint story misrepresents the tradition and should be reclassified or replaced by the sovereign_override_reading. If immutability is live within the tradition, this reading is accurately capturing one branch of the kernel contest. This affects the genealogy of false summits: a fabricated natural-law claim is a false summit by definition; a genuinely contested claim within the tradition is a site of contestation, not a lie.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immutable_mandate_reading_credibility, conceptual, 'Whether the immutable-mandate reading faithfully represents the tradition or misrepresents it for contemporary legitimation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t5, salic_prohibition__immutable_mandate_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(sali_tr_t5, observed).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__immutable_mandate_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(sali_tr_t10, observed).
narrative_ontology:measurement(sali_tr_t15, salic_prohibition__immutable_mandate_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(sali_tr_t15, observed).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__immutable_mandate_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(sali_tr_t20, observed).
narrative_ontology:measurement(sali_tr_t25, salic_prohibition__immutable_mandate_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(sali_tr_t25, observed).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__immutable_mandate_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(sali_tr_t30, observed).
narrative_ontology:measurement(sali_tr_t35, salic_prohibition__immutable_mandate_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(sali_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t5, salic_prohibition__immutable_mandate_reading, base_extractiveness, 5, 0.67).
narrative_ontology:measurement_basis(sali_be_t5, observed).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__immutable_mandate_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(sali_be_t10, observed).
narrative_ontology:measurement(sali_be_t15, salic_prohibition__immutable_mandate_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement_basis(sali_be_t15, observed).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__immutable_mandate_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(sali_be_t20, observed).
narrative_ontology:measurement(sali_be_t25, salic_prohibition__immutable_mandate_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(sali_be_t25, observed).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__immutable_mandate_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(sali_be_t30, observed).
narrative_ontology:measurement(sali_be_t35, salic_prohibition__immutable_mandate_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(sali_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t5, salic_prohibition__immutable_mandate_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement_basis(sali_su_t5, observed).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__immutable_mandate_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement_basis(sali_su_t10, observed).
narrative_ontology:measurement(sali_su_t15, salic_prohibition__immutable_mandate_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement_basis(sali_su_t15, observed).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__immutable_mandate_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement_basis(sali_su_t20, observed).
narrative_ontology:measurement(sali_su_t25, salic_prohibition__immutable_mandate_reading, suppression_requirement, 25, 0.89).
narrative_ontology:measurement_basis(sali_su_t25, observed).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__immutable_mandate_reading, suppression_requirement, 30, 0.89).
narrative_ontology:measurement_basis(sali_su_t30, observed).
narrative_ontology:measurement(sali_su_t35, salic_prohibition__immutable_mandate_reading, suppression_requirement, 35, 0.89).
narrative_ontology:measurement_basis(sali_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__immutable_mandate_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% The Salic prohibition kernel (constraint_family: salic_prohibition) decomposes into three structurally distinct constraint readings. The immutable_mandate_reading (THIS constraint) treats the prohibition as irrevocable natural/divine law; the sovereign_override_reading treats it as revocable positive law; the cognatic_reversion_reading treats it as inapplicable to non-Frankish contexts. Each reading has different ε, different beneficiary/victim structures, and different extraction profiles. The readings compete: adoption of one reading's framing delegitimizes the others in the same jurisdiction. All three are linked via network.affects_constraints because the legitimacy of each reading depends on the viability of the others (if immutability is falsified, sovereignty override becomes more plausible; if cognatic systems prove stable elsewhere, immutability claims lose credibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, powerful, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
