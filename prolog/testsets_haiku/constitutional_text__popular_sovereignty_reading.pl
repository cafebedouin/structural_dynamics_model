% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Authority Derives from Popular Sovereignty
 *   domain: constitutional_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the popular sovereignty reading of the
 *   constitutional text kernel. The reading claims that ultimate authority
 *   over constitutional meaning resides in the constituent power of the demos
 *   — expressed through amendment, convention, or revolutionary mobilization
 *   — rather than in courts or legislatures. This creates a tangled_rope
 *   structure: it coordinates a mechanism for popular reinterpretation and
 *   bypass of institutional procedures (genuine coordination function) while
 *   simultaneously extracting authority from institutional specialists and
 *   destabilizing procedural norms that depend on expertise and stability
 *   (asymmetric extraction, requiring active suppression of institutional
 *   resistance). The claim/metric gap reflects the reading's own structural
 *   position: it claims to be about legitimate democratic authority, yet
 *   operates by subordinating institutional authority to extra-institutional
 *   mobilization — a structure that benefits popular movements while harming
 *   institutional predictability.
 *
 * KEY AGENTS:
 *   - Popular mobilization (social movements, conventions, amendment campaigns) — structurally positioned as the beneficiary of popular sovereignty doctrine; gains authority to reinterpret constitutional meaning outside institutional channels
 *   - Judiciary — structurally positioned as payer/constrained agenda-setter; must administer law under a doctrine that denies them final interpretive authority
 *   - Legislature — structurally positioned as payer/constrained agenda-setter; subordinated to constituent power that can override legislative choices through amendment
 *   - Constitutional amendment movements — explicitly benefit from doctrine that legitimizes amendment as the proper vehicle for constitutional change
 *   - Institutional specialists (judges, constitutional lawyers, legal scholars) — positioned as payer; their expertise authority erodes under popular sovereignty framing
 *   - Judicial expertise establishment — identity-locked payer; the institution exists to interpret constitutionally but doctrine denies it that authority
 *   - Legislative procedural stability — identity-locked payer; formal procedure is undermined by permanent popular override authority
 *   - Minority protections apparatus — excluded; trapped because it depends on institutional stability the doctrine destabilizes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.38).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.72).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Authority Derives from Popular Sovereignty").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '607f8cca-1261-45b2-93c1-bb03d4bfb63a').
narrative_ontology:cs_kernel_codification('607f8cca-1261-45b2-93c1-bb03d4bfb63a', formalized).
narrative_ontology:cs_authority_grounding('607f8cca-1261-45b2-93c1-bb03d4bfb63a', lineage).
narrative_ontology:cs_interpretation_layer_present('607f8cca-1261-45b2-93c1-bb03d4bfb63a').
narrative_ontology:cs_reading_relation('607f8cca-1261-45b2-93c1-bb03d4bfb63a', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('607f8cca-1261-45b2-93c1-bb03d4bfb63a', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('607f8cca-1261-45b2-93c1-bb03d4bfb63a', foundational, constituent_power_ultimate_authority).
narrative_ontology:cs_axiom_status(constituent_power_ultimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('607f8cca-1261-45b2-93c1-bb03d4bfb63a', constituent_power_ultimate_authority, deontological).
narrative_ontology:cs_axiom('607f8cca-1261-45b2-93c1-bb03d4bfb63a', secondary, institutional_authority_subordinate_to_mobilization).
narrative_ontology:cs_axiom_status(institutional_authority_subordinate_to_mobilization, holdable).
narrative_ontology:cs_axiom_grounding('607f8cca-1261-45b2-93c1-bb03d4bfb63a', institutional_authority_subordinate_to_mobilization, deontological).
narrative_ontology:cs_reference_frame('607f8cca-1261-45b2-93c1-bb03d4bfb63a', constituent_power_sovereignty_framework).
narrative_ontology:cs_drift_state('607f8cca-1261-45b2-93c1-bb03d4bfb63a', contemporary_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('607f8cca-1261-45b2-93c1-bb03d4bfb63a', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_mobilization).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, constitutional_amendment_movements).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_specialists).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_expertise_establishment).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_procedural_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislature).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, people_as_ultimate_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social movements, constitutional conventions, and electoral mandates that claim to speak for the people's constituent power. Benefits from a reading that positions popular mobilization as the final arbiter of constitutional meaning, enabling mass-scale reinterpretation outside formal institutions. Can mobilize, demobilize, or redirect pressure toward courts or legislature depending on perceived responsiveness.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, popular_mobilization, beneficiary,
    powerful, generational, mobile, national).

% Courts that must decide cases and claim interpretive authority over constitutional text. Under popular sovereignty framing, their decisions are subordinate to constituent power — a court's ruling can be overridden by popular amendment or convention. Must administer law while watching legitimacy erode when popular opinion diverges from their interpretation. Constrained because courts cannot exit their constitutional role without institutional collapse.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, judiciary, agenda_setter).

% Parliament that must enact law within the framework of constitutional limits, yet under popular sovereignty doctrine is subordinate to constituent power. Cannot claim final authority over constitutional meaning; popular amendment can reverse legislative choices. Like courts, constrained because exit is institutional suicide — legislatures depend on the constitutional framework they would need to abandon.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, legislature, agenda_setter).

% Organizations explicitly pursuing constitutional amendment. Benefits directly from popular sovereignty doctrine because it legitimizes their claim to represent constituent power and positions amendment as the proper vehicle for constitutional reinterpretation. Mobilize voters, collect signatures, build political coalitions, and exit if the political window closes (though the doctrine itself claims permanent residence in constitutional authority).
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_amendment_movements, beneficiary,
    organized, biographical, mobile, national).

% Constitutional lawyers, judges, legal scholars, and legislative staff whose professional authority rests on expertise in constitutional text and doctrine. Under popular sovereignty framing, their interpretations are subordinate to popular will — a constraint on their epistemic authority. They pay in reduced professional prestige and institutional autonomy when popular movements overturn their considered judgments.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_specialists, payer,
    powerful, biographical, constrained, national).

% The institutional apparatus of courts, bar associations, and legal education that claims expertise in constitutional interpretation. Identity-locked because the institution exists to interpret law authoritatively; popular sovereignty doctrine denies them that authority. Cannot exit without ceasing to be what they are, yet the doctrine implies they should defer to popular mobilization even when their expertise suggests error.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_expertise_establishment, payer,
    institutional, generational, identity_locked, national).

% The institutional procedures and norms that enable legislatures to function predictably — committees, parliamentary procedure, legislative calendars, separation of powers. Popular sovereignty doctrine threatens procedural stability by positioning popular amendment as co-equal or superior to legislative choice, creating permanent pressure to bypass formal procedures in the name of constituent power. Identity-locked because legislatures cannot function without procedure; the doctrine undermines procedure from outside the legislative framework.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislative_procedural_stability, payer,
    institutional, generational, identity_locked, national).

% Formal party structures that coordinate electoral mobilization and legislative action. Excluded from framing the constituent power doctrine — they are organizational apparatus, not the demos itself. Under popular sovereignty reading, they are supposed to be mere transmission belts for popular will, not architects of constitutional interpretation. Their exclusion is permanent because the doctrine claims the people (not organized representatives) are supreme.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, political_parties, excluded,
    powerful, generational, constrained, national).

% Groups that organize or would organize constitutional conventions — the formal mechanism through which constituent power is supposed to express itself. They benefit from popular sovereignty doctrine because it positions conventions as the authoritative site for constitutional reinterpretation. Can mobilize support for convention calls and exit if political conditions shift.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_convention_operators, agenda_setter,
    organized, biographical, mobile, national).

% Constitutional provisions, institutions, and doctrines designed to protect minority rights against majoritarian override. Excluded from benefiting under popular sovereignty framing because the doctrine privileges popular will (often majoritarian) over entrenched protections. Trapped because they depend on the same constitutional framework that popular sovereignty doctrine claims can be rewritten by the people at will.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, minority_protections_apparatus, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, popular_mobilization).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for constitutional reinterpretation and amendment that does not depend on judicial or legislative permission. Solves the coordination problem of how popular will can change constitutional meaning without waiting for institutions to agree — enables collective action through amendment conventions and mobilization campaigns.
% TRANSFER_FUNCTION: Moves interpretive authority from institutional specialists (courts and legislatures) to popular mobilization. Transfer is diffuse rather than to a named beneficiary: constitutional meaning becomes subject to whatever interpretation popular movements can mobilize around, rather than confined to expert interpretation or institutional procedure.
% ABSENT_VOICES: Minority protections advocates who fear majoritarian override; constitutional stability advocates who see amendment as disruptive; legislative specialists whose procedural norms are sidelined in favor of convention-based reinterpretation. These parties would argue for institutional boundaries and expert authority but are structurally excluded from the popular sovereignty framing itself.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty doctrine disappeared from constitutional discourse, courts and legislatures would regain primary interpretive authority; constitutional meaning would stabilize around institutional positions; amendment would be harder to justify outside formal procedures. The world would reorganize around institutional expertise rather than popular mobilization as the engine of constitutional change.
% FOUNDING_PROBLEM: How can constitutional authority survive when institutions (courts and legislatures) disagree with popular will? How can the people retain ultimate control over their fundamental law rather than becoming passive subjects of institutional interpretation?
% FOUNDING_PROBLEM_CORROBORATION: Popular mobilization movements, constitutional scholars in democratic theory, and international observers attesting to instances where popular will diverged from institutional interpretation and popular movements claimed authority to override it. Academic literature on constituent power and democratic theory outside the institutional establishment supports the founding problem's framing.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the doctrine's core claim—that popular will should govern constitutional meaning—is genuinely contestable rather than obviously false. Courts and legislatures retain significant procedural authority; they are not entirely stripped of power. However, the doctrine subordinates them in legitimacy terms, creating ongoing pressure to defer to popular mobilization. Theater ratio is high (0.68) because popular sovereignty invocations often carry theatrical weight: politicians claim to represent constituent power without actual mobilization; amendment is invoked as a threat more often than executed; conventions are called rarely. Much of the constraint's operation is performative — the threat of popular override disciplining institutions more than actual popular action does. Suppression is high (0.72) because maintaining the doctrine requires actively suppressing institutional resistance: courts and legislatures must suppress their own claims to authority; minority-protection doctrines must be subordinated to majoritarian framing; procedural norms must be treated as subordinate to popular will. The measurement series show theater rising (from 0.52 to 0.68) — over time, the performative use of popular sovereignty language grows while actual amendment activity remains stable. Extractiveness rises more slowly (0.22 to 0.38) because the doctrine's grip tightens gradually as courts internalize subordination and legislatures defer more frequently. Suppression requirement rises steadily (0.58 to 0.72) as institutions must increasingly suppress institutional claims to authority.
 *
 * PERSPECTIVAL GAP:
 *   From the popular mobilization seat, the doctrine is liberating: it positions the people as ultimate authority, enabling challenge to institutional excess. From the institutional specialist seat, it is eroding: the doctrine subordinates expertise and predictability to mobilization. From the legislative procedure seat, it is destabilizing: formal rules are constantly subject to popular override. The engine computes these divergent directionalities from the structural data: beneficiaries sit near d=0.0 (full benefit without bearing cost of enforcement), payers sit near d=1.0 (bearing cost of subordination without capturing benefit), constrained institutional actors sit near d=0.7-0.8 (forced to administer under subordinating doctrine). A court computing its own experience would see the constraint as high extraction (suppression + erosion of authority); popular movements would see coordination benefit (ability to mobilize for reinterpretation).
 *
 * DIRECTIONALITY LOGIC:
 *   Popular mobilization and amendment movements are declared beneficiaries because the doctrine legitimizes their claims to authority and creates procedural pathways (conventions) for them to reinterpret constitutional meaning. They face low suppression (can mobilize openly, claim democratic legitimacy) and have mobile exit (can redirect toward electoral politics if conventions fail). Judiciary and legislature are declared payers because they must subordinate their authority claims and actively suppress institutional resistance to maintain legitimacy. They are identity-locked (cannot exit their constitutional role) and highly constrained (must administer law under a doctrine denying them final authority). Institutional specialists and expertise establishments are payers because the doctrine erodes their epistemic authority: their interpretations are subordinated to popular will. The measurement series track how suppression requirement increases — institutions must suppress more institutional claims to authority as the doctrine becomes normalized. Theater ratio rises because the performative use of popular sovereignty language (invoking constituent power in electoral campaigns, threatening amendment) grows while actual amendment activity remains rare.
 *
 * MANDATROPHY ANALYSIS:
 *   Popular sovereignty doctrine avoids misclassification as pure extraction (snare) by carrying a genuine coordination function: it enables constitutional change outside rigid procedural channels, solving the real problem of how constitutions become obsolete when institutions cannot adapt. It avoids misclassification as pure coordination (rope) by operating through asymmetric extraction: the doctrine benefits mobilized movements while harming institutional stability and expertise authority. The tangled_rope classification captures both: the coordination problem (how to enable popular reinterpretation) AND the extraction problem (subordinating institutional authority to popular will) are solved by the SAME structural mechanism (declaring constituent power supreme). The measurement data show why mandatrophy is relevant: if founding_problem status is 'live' (constitutions do become obsolete), the constraint carries a real coordination function; but if extractiveness and theater ratio continue rising while actual amendment remains rare, the constraint may be devolving into pure theater (piton) — institutional authority subordinated to a popular mobilization threat that rarely activates. The theater_ratio trajectory (0.52 → 0.68) suggests theatrical maintenance is increasing, creating piton risk if the founding problem becomes 'dead' (institutions adequately adapt through reinterpretation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_power_empirical_activation,
    'Does ''constituent power'' refer to an actual capacity for coordinated popular reinterpretation, or is it a legitimating myth invoked by institutional actors to discipline each other?',
    'Empirical tracking: count instances where actual popular mobilization (amendments, conventions, mass movements) overrides institutional interpretation versus instances where the threat of constituent power is invoked but never activated. Measure amendment frequency, convention calls, and successful popular override of institutional decisions.',
    'If constituent power is regularly activated (high amendment rate, frequent conventions, successful popular override), the constraint coordinates genuine popular authority. If constituent power is invoked as threat but rarely activated, the constraint is becoming piton — institutional discipline through theatrical invocation of absent popular mobilization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constituent_power_empirical_activation, empirical, 'Whether constituent power doctrine describes actual popular mobilization capacity or theatrical institutional discipline.').

omega_variable(
    institutional_resistance_internalization,
    'Do courts and legislatures suppress their own authority claims because they genuinely accept popular sovereignty doctrine as legitimate, or because they are institutionally trapped and must administer under the doctrine while maintaining rhetorical deference?',
    'Analyze judicial opinions and legislative debates for expressions of authority claim versus expressions of deference. Track whether deference increases in moments of high popular mobilization (genuine subordination) versus in normal politics (rhetorical cover). Examine whether institutional actors attempt to reassert authority when popular pressure recedes.',
    'If suppression is internalized (institutions accept the doctrine), it becomes more stable and the constraint approaches rope-level coordination. If suppression is merely rhetorical cover for institutional resistance, the constraint is unstable and depends entirely on continued active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_resistance_internalization, conceptual, 'Whether institutional suppression of authority claims is internalized or performed for cover.').

omega_variable(
    minority_protection_irreconcilability,
    'Can constitutional protections for minorities (property rights, religious liberty, due process) coexist with authentic constituent power doctrine, or do these commitments logically foreclose each other?',
    'Test cases where popular mobilization seeks to override minority protections (banning a religion, seizing property, denying due process to a group). Examine whether courts and legislatures position themselves as defenders of minority rights against constituent power or as subordinate to constituent power in these contexts.',
    'If minority protections are treated as pre-political or entrenched against constituent power, the doctrine is not authentically operative — courts/legislatures claim this higher authority. If constituent power is allowed to override minority protections, the doctrine is authentic but deeply extraction-heavy (victims are minorities trapped by majoritarian override). This omega addresses whether the doctrine contains an internal contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_protection_irreconcilability, conceptual, 'Whether constituent power doctrine can authentically accommodate entrenched minority protections or whether they logically foreclose each other.').

omega_variable(
    sibling_reading_coexistence,
    'Is this popular sovereignty reading a description of how constitutional authority actually works, or is it one normative framing coexisting with judicial supremacy and legislative sovereignty as competing institutional cultures?',
    'Examine constitutional practice across jurisdictions and over time. Instances where courts claim final authority (judicial supremacy), legislatures claim override capacity (legislative sovereignty), and popular movements claim constituent power (popular sovereignty) all appear in real practice. Determine whether one reading is universally dominant or whether different readings prevail in different contexts/institutions.',
    'If one reading is universally dominant and stable, the others are defeated and the constraint has clear type. If readings coexist as institutional cultures (different courts/legislatures/movements operate under different readings), the constraint is fundamentally contested and may be unclassifiable at the system level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether popular sovereignty is a dominant reading or one coexisting with rival institutional readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__popular_sovereignty_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__popular_sovereignty_reading, theater_ratio, 16, 0.62).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__popular_sovereignty_reading, theater_ratio, 24, 0.65).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__popular_sovereignty_reading, theater_ratio, 32, 0.67).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_text__popular_sovereignty_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text__popular_sovereignty_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_text__popular_sovereignty_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_text__popular_sovereignty_reading, base_extractiveness, 32, 0.37).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_text__popular_sovereignty_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text__popular_sovereignty_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_text__popular_sovereignty_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_text__popular_sovereignty_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The constitutional_text kernel admits three structurally distinct constraint readings: popular_sovereignty_reading (this file) treats ultimate authority as constituent power of the demos; judicial_supremacy_reading treats it as vested in courts; legislative_sovereignty_reading treats it as vested in legislatures. Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, different temporal dynamics. They form a constraint family linked through network.affects_constraints. The popular sovereignty reading differs from siblings in that it subordinates institutional authority (courts and legislatures) to extra-institutional mobilization, creating extraction from institutional specialists while enabling coordination for popular movements. Judicial supremacy vests authority in courts (extraction from non-expert actors, coordination for rule-of-law beneficiaries). Legislative sovereignty vests it in legislatures (extraction from minorities subject to legislative override, coordination for procedural actors). All three readings coexist as live institutional cultures and are not merged into one story — ε-invariance principle (DP-001) requires decomposition whenever different observable framings produce substantially different extraction metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
