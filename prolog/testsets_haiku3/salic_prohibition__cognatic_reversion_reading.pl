% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law as Anachronistic Frankish Rule Inapplicable to Non-Frankish Territories
 *   domain: constitutional_law/dynastic_succession
 *
 * SUMMARY:
 *   Salic Law, a code of the Frankish confederation establishing male-only
 *   succession (agnatic primogeniture), persisted as a succession principle
 *   across territories annexed to Frankish dominion. This reading — the
 *   cognatic reversion reading — asserts that Salic Law was a Frankish custom
 *   never properly binding outside the Frankish homeland and that territorial
 *   law (cognatic primogeniture, female succession eligibility) prevails in
 *   non-Frankish territories. The constraint is CLAIMED as tangled_rope
 *   (coordination function: settle succession disputes; extraction: transfers
 *   rights to agnatic males at the expense of female heirs and territorial
 *   autonomy). Metrics are authored independently: extractiveness rises
 *   gradually (0.48→0.62) as the reading accumulates legal precedent and
 *   institutional support; theater_ratio plateaus around 0.41 (enforcement
 *   performance consists of scribal affirmation and ecclesiastical rulings,
 *   with genuinely novel jurisdictional work declining after early adoption);
 *   suppression_requirement stabilizes at 0.58 (the reading must actively
 *   exclude Frankish universalizers and agnatic purists to maintain its scope
 *   limit, but does not require force against territory-sovereignty
 *   advocates).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.58).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law as Anachronistic Frankish Rule Inapplicable to Non-Frankish Territories").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '71c257d9-547f-456d-be44-56ef475e6122').
narrative_ontology:cs_kernel_codification('71c257d9-547f-456d-be44-56ef475e6122', fixed_text).
narrative_ontology:cs_authority_grounding('71c257d9-547f-456d-be44-56ef475e6122', lineage).
narrative_ontology:cs_interpretation_layer_present('71c257d9-547f-456d-be44-56ef475e6122').
narrative_ontology:cs_reading_relation('71c257d9-547f-456d-be44-56ef475e6122', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('71c257d9-547f-456d-be44-56ef475e6122', salic_prohibition__sovereign_override_reading, influences).
narrative_ontology:cs_axiom('71c257d9-547f-456d-be44-56ef475e6122', foundational, territorial_origin_limits_custom_scope).
narrative_ontology:cs_axiom_status(territorial_origin_limits_custom_scope, holdable).
narrative_ontology:cs_axiom_grounding('71c257d9-547f-456d-be44-56ef475e6122', territorial_origin_limits_custom_scope, deontological).
narrative_ontology:cs_axiom('71c257d9-547f-456d-be44-56ef475e6122', foundational, local_law_supersedes_foreign_tribal_doctrine).
narrative_ontology:cs_axiom_status(local_law_supersedes_foreign_tribal_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('71c257d9-547f-456d-be44-56ef475e6122', local_law_supersedes_foreign_tribal_doctrine, conventional).
narrative_ontology:cs_reference_frame('71c257d9-547f-456d-be44-56ef475e6122', regional_legal_autonomy_framework).
narrative_ontology:cs_drift_state('71c257d9-547f-456d-be44-56ef475e6122', mid_medieval_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71c257d9-547f-456d-be44-56ef475e6122', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, cognatic_succession_advocates).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, territorial_integrity_champions).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, agnatic_purist_dynasties).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, male_primogeniture_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, female_heirs_and_dynastic_branches).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, regional_ecclesiastical_authorities).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_sovereignty_over_lineage_purity).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_primogeniture_as_legitimate_succession_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal scholars, regional nobility, and dynastic branches (particularly those with female heirs) who argue that Salic Law's male-only succession rule is a relic of Frankish tribal custom that never acquired universal binding force outside the Frankish heartland. They frame cognatic primogeniture (eldest child regardless of sex) as the legitimate succession framework for territories outside the original Frankish jurisdiction. They benefit by establishing a precedent that territorial law supersedes agnatic doctrine and that female succession is legally permissible and constitutionally sound.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_succession_advocates, beneficiary,
    organized, generational, mobile, continental).

% Rulers of non-Frankish territories (Aquitaine, Burgundy, Occitania, Iberian kingdoms) who invoke this reading to assert that their own territorial law takes precedence over a foreign kingdom's dynastic custom. They claim sovereignty over succession rules within their borders and resist incorporation of Salic Law as a universal standard. The reading strengthens their negotiating position in dynastic disputes and protects territorial autonomy.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, territorial_integrity_champions, beneficiary,
    institutional, generational, constrained, continental).

% Frankish kings, their cadet branches, and allied Frankish nobility whose dynastic legitimacy and inheritance claims rest on Salic Law's agnatic rule. They bear the cost of this reading when it restricts the application scope of Salic Law to the original Frankish territories, eroding their ability to impose it as a universal succession standard across conquered or allied lands. Their claims over non-Frankish territories become contestable if local succession law is privileged over Salic purity.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_purist_dynasties, payer,
    powerful, generational, constrained, continental).

% Male heirs and legal theorists committed to agnatic succession as the sole legitimate dynastic framework. They pay the cost of this reading when it legitimizes female succession alternatives and reduces the universality of male-only rules. Their inheritance claims become secondary to eldest-child claims under cognatic rules, and their ideological authority over succession questions is diminished.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, male_primogeniture_defenders, payer,
    powerful, biographical, mobile, continental).

% Daughters and sisters of ruling families in non-Frankish territories who would have been excluded from succession under strict Salic application but are included under cognatic primogeniture. They benefit directly from this reading's affirmation of female succession legitimacy. They remain partially excluded from the broader debate because the decision about which reading applies is made by male dynastic actors and ecclesiastical authorities without systematic female participation.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs_and_dynastic_branches, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, female_heirs_and_dynastic_branches, excluded).

% The Frankish Church and papal-adjacent authorities who have the capacity to rule on the scope of Salic Law's binding force. They mediate dynastic disputes through canon law and set precedent on succession legitimacy. In this reading they are positioned as capable of ruling that Salic Law's authority is limited to its origin context and that territorial law prevails in non-Frankish lands. Their rulings can enforce or defer this reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, frankish_ecclesiastical_authority, agenda_setter,
    institutional, generational, constrained, continental).

% Local bishops and regional church councils in Aquitaine, Burgundy, Occitania, and other non-Frankish territories who benefit from this reading because it affirms their own jurisdictional authority over dynastic succession questions within their regions. The reading reinforces their position against Frankish ecclesiastical centralization and strengthens local ecclesiastical authority over regional succession disputes.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, regional_ecclesiastical_authorities, beneficiary,
    organized, generational, constrained, regional).

% The institution of Frankish kingship as such, which has accumulated claims to rule over non-Frankish territories through conquest and marriage alliance. Under this reading, Frankish kingship loses universalizing power over succession law in those territories. The institution's authority depends on subordinating local custom to Frankish rule; the reading's affirmation of local legal autonomy reduces Frankish institutional reach.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, frankish_kingship_continuity, agenda_setter,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(salic_prohibition__cognatic_reversion_reading, frankish_kingship_continuity).

% Historical analysts and legal theorists outside dynastic interest (scribes, chroniclers from neutral territories, later historians) who examine which reading is most coherent with evidence: whether Salic Law was ever intended as a universal rule or remained a specifically Frankish custom. Their testimony and analysis feed the corroboration layer for the founding problem and its status.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, observer_secular_authority, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, agnatic_purist_dynasties).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision rule for dynastic succession disputes in multi-territorial realms: Salic Law offers one framework (agnatic, male-only); cognatic primogeniture offers another. This reading coordinates around the principle that territorial law and cognatic inheritance prevail outside the Frankish heartland, resolving jurisdictional ambiguity about which succession standard applies in annexed or allied territories.
% TRANSFER_FUNCTION: Transfers legitimacy and inheritance rights. Under Salic Law, male heirs gain exclusive succession rights; females are excluded. Under cognatic primogeniture, the eldest child (regardless of sex) inherits. This reading shifts succession rights from Salic beneficiaries (agnatic males, Frankish-centered authority) to cognatic succession advocates and female heirs. The transfer also moves jurisdictional authority from Frankish-centralized law to territorial-local law in non-Frankish lands.
% ABSENT_VOICES: Female dynastic actors are substantially excluded from the debate about succession rules; their interests are spoken for by their male relatives and by ecclesiastical authorities, not authored by themselves. Conquered or allied non-Frankish populations (commoners, local nobility outside dynastic circles) are absent from formal succession deliberation, though territorial integrity champions claim to speak for regional sovereignty interests.
% DISAPPEARANCE_RATIONALE: If this reading — that Salic Law applies only to Frankish territories and cognatic succession is legitimate elsewhere — disappeared and Salic Law's agnatic rule became the universal dynastic standard across all territories, inheritance sequences would reorganize: female heirs would lose their places in succession lines across non-Frankish kingdoms, territorial rulers would lose local legal authority over succession, and the geographic reach of Frankish dynastic law would expand at the expense of regional autonomy. Conversely, if this reading becomes the binding standard, non-Frankish territories regain succession autonomy, female heirs gain legal standing, and regional law supersedes external dynastic doctrine.
% FOUNDING_PROBLEM: Salic Law emerged as a rule of the Frankish tribal confederation and was codified in Frankish legal tradition. As Frankish kings conquered and annexed non-Frankish territories (Aquitaine, Burgundy, Occitania, parts of Italia), the question arose: does Salic Law bind succession in these territories? If yes, local custom is overridden. If no, territorial law prevails. This ambiguity created dynastic disputes: a daughter's claim to an Aquitanian county under local cognatic rules collided with Frankish claims that Salic Law barred her. The founding problem is the collision between two succession systems with overlapping territorial reach but different beneficiary structures.
% FOUNDING_PROBLEM_CORROBORATION: Frankish kingship and agnatic purists attest that Salic Law is a universal principle binding on all territories under Frankish sovereignty. Regional rulers, non-Frankish nobility, and cognatic succession advocates attest that Salic Law is a Frankish custom inapplicable outside Frankish lands and that territorial law must prevail. Ecclesiastical authorities from non-Frankish regions attest that local jurisdiction over succession should be respected. Contemporary legal historians (scribal tradition from neutral territories, later chroniclers outside the Frankish interest) note that early Salic Law texts show no evidence of intended universal application — the custom emerged in a specific geographic and tribal context. The founding problem is corroborated as real and unsettled by all parties.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 terminal) because the reading benefits cognatic advocates and territorial rulers at the cost of agnatic males and Frankish-centered authority, but the transfer is not total extraction — it is a reallocation of jurisdictional authority and succession rights, not a pure rent. Suppression is moderate (0.58) because the reading's persistence requires active institutional defense (ecclesiastical rulings, scribal precedent) against Frankish claims to universality, but the defense is primarily doctrinal rather than coercive. Theater is intermediate (0.41): the reading involves genuine jurisdictional work (determining which law applies in mixed-heritage territories), but a growing share of activity is ceremonial (repeating the scope-limit principle in inheritance ceremonies, ecclesiastical councils) rather than novel dispute resolution. The measurement grid is aligned across all three metrics at every time point; extraction accumulation is the primary drift signal (rising from 0.48 to 0.62 over 25 years, then plateauing), indicating the reading stabilizes after achieving institutional consensus. Suppression requirement rises along with extractiveness but more slowly, suggesting the reading's opponents (Frankish universalizers) lose capacity to resist as the reading consolidates institutional backing.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Frankish ecclesiastical authority, regional rulers), this reading establishes jurisdictional clarity and affirms their authority to rule on succession — a coordination benefit. From the payer seat (agnatic male heirs, Frankish kingship), the reading erodes the universalizing reach of Salic Law and restricts their inheritance claims in non-Frankish territories — extraction. The cognatic beneficiary seat (female heirs, regional territorial champions) experiences this as a gain in legal standing and sovereignty, not extraction. The engine computes directionality per-seat from these structural differences: female heirs and regional rulers show low d (beneficiaries, exit opportunities exist within the territorial-law frame); agnatic purists show high d (targets, constrained to defend Salic universality); ecclesiastical authorities show intermediate d (coordinators with enforcement power, not pure beneficiaries or victims).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary group 'cognatic_succession_advocates' includes female heirs (who gain inheritance standing), regional rulers (who gain jurisdictional autonomy), and legal scholars supporting territorial law — they benefit from the scope limitation of Salic Law to the Frankish homeland. Victim group 'agnatic_purist_dynasties' includes Frankish kingship branches, agnatic male heirs disadvantaged by cognatic rules, and universalist legal theorists — they lose when Salic Law's reach is limited. The constraint's operation requires active ecclesiastical and scribal enforcement (maintaining the scope doctrine in legal proceedings, teaching it in cathedral schools, invoking it in succession disputes), which places the constraint in tangled_rope (coordination + extraction + enforcement). The accessibility_collapse (0.68) reflects that once the territorial-sovereignty principle is established and ecclesiastically affirmed, alternatives (pure universality of Salic Law, pure local custom without reference to any framework) collapse; the trade-off between jurisdictions is structurally hard to escape. Resistance (0.71) captures that agnatic purists and Frankish universalizers mount substantial ongoing resistance through counter-claims to Salic authority, though institutional support for the cognatic-reversion reading gradually concentrates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (collision of two succession systems with overlapping reach) remains structurally live in this reading. However, there is a drift toward mandatrophy: over the measurement interval, extractiveness plateaus (0.62 for 10+ years at the terminal end) even as theater_ratio stabilizes. This suggests the constraint has shifted from solving the founding problem (determining which law applies) toward performing the solution (repeating the scope-limit principle ceremonially). If the theatrical maintenance (ecclesiastical councils reaffirming the principle without novel cases) continues to grow relative to active dispute resolution, the reading risks degrading into a piton — a constraint persisting by institutional inertia after its coordinate function atrophies. The commentary records this trajectory: theater starts at 0.28 and rises to 0.41, but extraction does not rise proportionally (extraction plateaus while theater plateaus), suggesting the reading has captured enough institutional consensus that enforcement becomes ritualized. A future analysis should monitor whether theater_ratio climbs above 0.55 (indicating performance exceeds function) while extraction remains flat — that pattern marks the transition to piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salic_anachronism_vs_divine_law,
    'Is Salic Law a contingent Frankish custom that happened to codify male-only succession, or a divinely ordained principle embedded in natural law and therefore universally binding?',
    'Examination of textual evidence: do the earliest Salic Law manuscripts claim universal divine grounding, or do they present the rule as a specifically Frankish convention? Does ecclesiastical authority (papal pronouncements, canon law, conciliar decrees) treat Salic succession as natural law or as positive law revisable by territorial sovereigns?',
    'If Salic Law is shown to be contingent Frankish custom, the cognatic_reversion_reading (this reading) is strengthened and the immutable_mandate_reading (sibling) is weakened. If foundational texts claim divine grounding, the immuable_mandate_reading gains support and this reading faces credibility pressure. Classification diverges: immutable_mandate reads as mountain (natural law, non-extractive); cognatic_reversion reads as tangled_rope (contestable jurisdictional coordination with asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(salic_anachronism_vs_divine_law, empirical, 'Whether Salic Law claims natural-law universality or Frankish-custom particularity.').

omega_variable(
    female_succession_legitimacy,
    'Is female succession through cognatic primogeniture a legitimate constitutional alternative to Salic agnatic rules, or does it violate fundamental principles of dynastic integrity?',
    'Historical evidence from non-Frankish territories (Aquitaine, Burgundy, Occitania, Iberian kingdoms) that practiced female succession before Salic Law was imposed: did female-headed dynasties produce stable governance, ecclesiastical recognition, and legal continuity, or did they face systematic delegitimization? Do later legal theorists (medieval canonists, Romanists) cite female succession precedent as legitimate, or dismiss it as aberrant?',
    'If female succession demonstrates governance legitimacy and historical precedent, the cognatic_reversion_reading is validated and female heirs gain legal standing. If female succession is shown to produce instability or is absent from historical record, the agnatic_purist_dynasties'' position is strengthened and this reading faces functional critique. The constraint''s claimed type remains tangled_rope, but extraction allocation shifts depending on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(female_succession_legitimacy, empirical, 'Whether female succession via cognatic primogeniture is historically and constitutionally legitimate.').

omega_variable(
    territorial_law_vs_universal_custom,
    'Is the jurisdictional doctrine of this reading — that territorial law prevails in non-Frankish territories — a genuine principle of medieval sovereignty, or a post-hoc rationalization for regional resistance to Frankish authority?',
    'Compare how this principle is applied across different conquest scenarios: does it apply consistently when regional law benefits Frankish dynasties (e.g., in a Frankish-friendly territory), or only when it constrains them? Do contemporary chroniclers and legal authorities invoke the principle equally in all territorial contexts, or selectively? Does the principle survive into later legal theory (natural-law treatises, sovereign-state doctrine), or does it fade as Frankish centralization increases?',
    'If the principle is applied consistently across contexts and survives in later theory, it represents genuine jurisdictional doctrine and the cognatic_reversion_reading is structurally sound. If applied selectively to constrain Frankish advantage, it is a rationalizing cover for asymmetric power negotiation — suggesting the constraint is more extractive than the tangled_rope reading claims. This affects both the classification confidence and the interpretation of suppression_requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_law_vs_universal_custom, conceptual, 'Whether territorial-law supremacy is a principled doctrine or strategic rationalization.').

omega_variable(
    identity_lock_female_heirs,
    'For female heirs identity-locked into dynastic membership by kinship and inheritance claim, does the availability of cognatic succession rules in this reading actually reduce suppression, or does it leave them locked in an identity-dependent subordination (subordination to dynastic males deciding when cognatic rules are ''applicable'')?',
    'Post-recognition measurement: if a female heir gains legal standing under cognatic primogeniture and then faces resistance from male kinfolk or ecclesiastical authorities who invoke counter-doctrines, does the reading''s legal framework protect her claim, or does suppression merely shift from formal legal exclusion to informal enforcement? Do female heirs who invoke cognatic rules experience acceptance or continued denial?',
    'If cognatic rules effectively protect female succession claims, the reading genuinely reduces suppression for that agent class. If the rules are invoked then overridden by counter-authority or informal resistance, suppression persists despite the reading''s formal affirmation — the reading becomes more theatrical and less functional. This affects measured suppression and theater_ratio interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_female_heirs, empirical, 'Whether cognatic succession rules reduce or merely relocate suppression of female heirs.').

omega_variable(
    reading_conflict_foreclosure,
    'Does the cognatic_reversion_reading''s core premise (Salic Law is anachronistic and inapplicable outside Frankish homeland) logically foreclose the immutable_mandate_reading (Salic Law is universal divine law), or can both readings coexist within different parties'' frameworks?',
    'Test whether a party can hold both premises simultaneously without logical contradiction: can one coherently assert that ''Salic Law is a Frankish custom that is nonetheless universally binding outside its origin context''? The answer determines whether the readings foreclose or coexist.',
    'If the readings truly foreclose, they cannot be held by the same authority system — only one can be institutionally validated. If they coexist, different authorities (regional vs. Frankish, local-church vs. papal-centered) can simultaneously endorse different readings. Coexistence means lower-confidence classification (both types remain live options); foreclosure means winner-take-most dynamics. This affects how mandatrophy pressure emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_conflict_foreclosure, conceptual, 'Whether the cognatic and immutable readings logically foreclose each other or can coexist across different authorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(sali_tr_t0, projected).
narrative_ontology:measurement(sali_tr_t5, salic_prohibition__cognatic_reversion_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(sali_tr_t5, observed).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__cognatic_reversion_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(sali_tr_t10, observed).
narrative_ontology:measurement(sali_tr_t15, salic_prohibition__cognatic_reversion_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(sali_tr_t15, observed).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__cognatic_reversion_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(sali_tr_t20, observed).
narrative_ontology:measurement(sali_tr_t25, salic_prohibition__cognatic_reversion_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(sali_tr_t25, observed).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__cognatic_reversion_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(sali_tr_t30, observed).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__cognatic_reversion_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(sali_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(sali_be_t0, projected).
narrative_ontology:measurement(sali_be_t5, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(sali_be_t5, observed).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(sali_be_t10, observed).
narrative_ontology:measurement(sali_be_t15, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(sali_be_t15, observed).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(sali_be_t20, observed).
narrative_ontology:measurement(sali_be_t25, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(sali_be_t25, observed).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(sali_be_t30, observed).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(sali_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(sali_su_t0, projected).
narrative_ontology:measurement(sali_su_t5, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(sali_su_t5, observed).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(sali_su_t10, observed).
narrative_ontology:measurement(sali_su_t15, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(sali_su_t15, observed).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(sali_su_t20, observed).
narrative_ontology:measurement(sali_su_t25, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(sali_su_t25, observed).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(sali_su_t30, observed).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(sali_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__cognatic_reversion_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% The salic_prohibition kernel family consists of three constraint stories, each instantiating a different reading of the contested kernel 'salic_prohibition'. This story (cognatic_reversion_reading) asserts Salic Law is anachronistic and territorially limited. The immutable_mandate_reading (sibling) asserts Salic Law is universal divine law. The sovereign_override_reading (sibling) asserts Salic Law is revocable positive law subject to sovereign authority. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and potentially different types. All three are linked via network.affects_constraints because the success of one reading constrains the viability of the others at the level of institutional authority and legal precedent. The readings occupy different parties' frameworks (Frankish universalists vs. territorial sovereigns vs. ecclesiastical reformers) and their relative institutional strength determines which constraints operationalize at any given historical moment. The kernel itself (Salic Law's binding force) is the contested commitment that the three readings contest; the three constraints are the structural manifestations of that contest instantiated across seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
