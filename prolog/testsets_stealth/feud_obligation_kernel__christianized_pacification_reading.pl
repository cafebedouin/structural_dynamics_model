% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification Regime over Blood-Feud Obligations (Divine-Law Reading)
 *   domain: legal_anthropology/medieval_history/comparative_politics
 *
 * SUMMARY:
 *   From the Peace of God councils at Charroux (989) onward, the Latin Church
 *   recast the blood-feud obligation — the customary duty of kin groups to
 *   avenge or compound killings of their members — as a violation of divine
 *   law ('vengeance is mine'), reserving lawful violence to God and to
 *   delegated ecclesiastical and royal offices. Over the following two and a
 *   half centuries this prohibition regime assembled real machinery: sworn
 *   peace oaths, truce windows, penitential tariffs for homicide,
 *   excommunication and interdict against defiant feuders, and, increasingly,
 *   crown criminal jurisdiction over killing. The arrangement genuinely
 *   shielded the classes the early statutes named and substituted payment for
 *   killing in thousands of cases; it simultaneously built a
 *   revenue-and-jurisdiction apparatus whose fees rode on every settlement,
 *   criminalized the honor codes that had bounded feud violence, and left
 *   feud practice alive beneath formal peace. EPSILON REFERENT: the standing
 *   arrangement under contest is this dual structure as it operated c.
 *   989-1250 — condemned feud practice persisting under a prohibition
 *   apparatus — assessed by this reading's own lights; the reading's endorsed
 *   ideal (a fully pacified Christendom under legitimate authority) is NOT
 *   the referent. FAMILY NOTE: the colloquial label 'blood feud' decomposes
 *   into three structurally distinct constraints sharing the
 *   feud_obligation_kernel; this file instantiates the
 *   christianized_pacification_reading, whose epsilon (0.62) reflects
 *   spiritual-peril and material burden borne by all participants plus
 *   church/royal rents, net of delivered protection. The sibling readings —
 *   stateless_coordination_reading (feud as self-enforcing justice absent
 *   central capacity; kin groups as net beneficiaries, epsilon far lower) and
 *   extraction_cycle_reading (feud as a destructive economic loop; no church
 *   beneficiary, no vindicated doctrine) — are separate constraint files
 *   linked through network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - latin_church_hierarchy: Primary agenda_setter
 *   (institutional/arbitrage) — legislates peace, fixes penitential tariffs,
 *   collects composition fees and endowments - royal_jurisdiction_officials:
 *   Secondary agenda_setter and beneficiary (institutional/arbitrage) —
 *   criminalizes homicide, collects murder fines - feud_bound_kin_groups:
 *   Primary target (organized/identity_locked) — bound to avenge or compound;
 *   bears the spiritual and material burden - wergild_debtor_households:
 *   Target (moderate/trapped) — composition debts mortgaging land for decades
 *   - peasant_communities_in_feud_zones: Protected-but-taxed seat
 *   (powerless/trapped) — receives peace protection, pays tithes and fees -
 *   customary_feud_arbiters: Excluded voice (moderate/identity_locked) —
 *   displaced settlement expertise - canon_law_commentators: Analytical
 *   observer (analytical/analytical) — sees doctrine, revenue, and
 *   enforcement gaps together
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.62).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.78).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification Regime over Blood-Feud Obligations (Divine-Law Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_politics").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '2c2c417e-5594-4221-97fb-2871d83be11c').
narrative_ontology:cs_kernel_codification('2c2c417e-5594-4221-97fb-2871d83be11c', fixed_text).
narrative_ontology:cs_authority_grounding('2c2c417e-5594-4221-97fb-2871d83be11c', lineage).
narrative_ontology:cs_interpretation_layer_present('2c2c417e-5594-4221-97fb-2871d83be11c').
narrative_ontology:cs_reading_relation('2c2c417e-5594-4221-97fb-2871d83be11c', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('2c2c417e-5594-4221-97fb-2871d83be11c', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('2c2c417e-5594-4221-97fb-2871d83be11c', foundational, vengeance_is_divine_prerogative_alone).
narrative_ontology:cs_axiom_status(vengeance_is_divine_prerogative_alone, holdable).
narrative_ontology:cs_axiom_grounding('2c2c417e-5594-4221-97fb-2871d83be11c', vengeance_is_divine_prerogative_alone, theological).
narrative_ontology:cs_axiom('2c2c417e-5594-4221-97fb-2871d83be11c', foundational, lawful_violence_flows_only_from_anointed_delegation).
narrative_ontology:cs_axiom_status(lawful_violence_flows_only_from_anointed_delegation, holdable).
narrative_ontology:cs_axiom_grounding('2c2c417e-5594-4221-97fb-2871d83be11c', lawful_violence_flows_only_from_anointed_delegation, theological).
narrative_ontology:cs_axiom('2c2c417e-5594-4221-97fb-2871d83be11c', secondary, penitential_discipline_suppresses_feud_impulse).
narrative_ontology:cs_axiom_status(penitential_discipline_suppresses_feud_impulse, holdable).
narrative_ontology:cs_axiom_grounding('2c2c417e-5594-4221-97fb-2871d83be11c', penitential_discipline_suppresses_feud_impulse, instrumental).
narrative_ontology:cs_reference_frame('2c2c417e-5594-4221-97fb-2871d83be11c', divine_delegated_violence_order).
narrative_ontology:cs_drift_state('2c2c417e-5594-4221-97fb-2871d83be11c', high_medieval_jurisdictional_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2c2c417e-5594-4221-97fb-2871d83be11c', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, latin_church_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_jurisdiction_officials).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_bound_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, wergild_debtor_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, peasant_communities_in_feud_zones).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, peasant_communities_in_feud_zones).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_law_prohibition_of_vengeance).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, delegated_violence_authority_doctrine).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, penitential_discipline_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds councils that legislate peace oaths (Peace of God, Truce of God), preaches against private vengeance, fixes penitential tariffs for homicide in its manuals, excommunicates feuders who defy settlement, and receives composition payments, tithes, and endowed altars funded by peace agreements. Claims sole authority to declare which killings God accepts. Its officers — bishops, abbots, papal legates — staff the tribunals that hear feud disputes. When a lineage refuses composition, the machinery available includes interdict and public penance; when a lineage complies, the church receives land, cash, and liturgical fees. Its reach crosses every realm of Latin Christendom, and its archives, schools, and preaching network are unmatched by any rival administration.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, latin_church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).

% Kings and their officers enforce the ban on private vengeance where their writ runs: homicide becomes a plea of the crown (Angevin England from Henry II onward), murder fines and amercements flow to the treasury, and breach of a sworn peace is prosecuted as contempt of the king's oath. They gain revenue and a monopoly claim on coercive force, but their capacity varies sharply by region — strong in Angevin England and Norman Sicily, thin east of the Elbe. They can also bargain, licensing reprisal between foreign merchants when diplomacy makes suppression inconvenient.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_jurisdiction_officials, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_jurisdiction_officials, beneficiary).

% Lineages bound by custom to pursue or compound every killing of a member. Under the older dispensation their honor, marriage prospects, and legal standing depended on visible willingness to avenge; under the prohibition the same act that preserves their standing brings excommunication and public penance. Most navigate between: compounding for cash where they must, keeping the feud alive in ritual insult and limited raiding where they can. Renouncing the obligation entirely means dishonor and exposure to predatory neighbors; fulfilling it means damnation as preached from the altar. Exit runs through generations — feud memory is inherited along with the land.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_bound_kin_groups, payer,
    organized, generational, identity_locked, regional).

% Households on the owing side of a killing, liable for compensation scaled to the victim's rank. A single slaying could mortgage land for decades; failure to pay converted the debt into a fresh grievance and renewed raiding. Church and royal courts increasingly took a cut of every composition as fee or fine, raising the total burden above what custom alone demanded.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, wergild_debtor_households, payer,
    moderate, biographical, trapped, local).

% Cultivators whose fields burn when neighboring lineages fight. They gained real protection from the peace-oath movements — clergy and peasants were the classes the earliest Peace statutes named — and they fund the apparatus through tithes. They pay twice: once in tithe and court fee, once in the harvests lost while enforcement fails. Their villages cannot move, and their protection depends on oaths sworn by lords they do not control.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, peasant_communities_in_feud_zones, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, peasant_communities_in_feud_zones, beneficiary).

% Kin-honor arbitrators, lawspeakers, and elder mediators who ran the older settlement process — assessing wergild, brokering truces, arranging compensated duel by proxy. The prohibition regime did not recruit them; church courts hired clerks trained in canon law instead. They regard the new tribunals as ignorant of the customs that kept feuds bounded, and their knowledge dies with them largely unrecorded.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, customary_feud_arbiters, excluded,
    moderate, biographical, identity_locked, regional).

% Decretists and decretalists who analyze when killing is lawful, what penance homicide requires, and whether princely war differs from private vengeance. They write the distinctions the tribunals apply and can see the whole structure at once: the doctrine, the revenue, the enforcement gaps, and the lineages caught between honor and salvation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, canon_law_commentators, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, latin_church_hierarchy).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem among horizontally armed kin groups: no lineage can safely renounce vengeance unilaterally, so a credible external guarantor — God, administered through anointed offices — lets many lineages stand down at once, substitutes scheduled composition for reciprocal killing, extends protection to classes that could not defend themselves (clergy, peasants, merchants), and provides adjudicating forums whose sanctions (excommunication, interdict, royal penalty) raise the cost of defection.
% TRANSFER_FUNCTION: Moves composition wealth (wergild payments routed through church and royal courts with fees attached), penitential tariffs and tithes, land endowments gifted at peace settlements, and murder fines into church treasuries and royal coffers; moves adjudication of killing from kin councils to clerical and royal fora; moves honor standing from vengeance-renown toward oath-compliance, with the church as certifier.
% ABSENT_VOICES: Customary feud arbiters and lineage heads who maintained that the older settlement customs kept feuds bounded and that the new tribunals neither knew nor respected those customs; unavenged heirs in cases where composition was pressured rather than consented; and communities in frontier zones never consulted by councils held in Gaul and Catalonia. None sit in the peace-council rolls, which record bishops, abbots, princes, and castellans.
% DISAPPEARANCE_RATIONALE: If the prohibition-and-delegation regime vanished overnight, kin vengeance obligations would reassert wherever enforcement lapsed (as they visibly did in frontier and island zones), church-court composition fees, penitential tariff income, and royal murder fines would evaporate, and dispute processing would reorganize around kin councils, wergild negotiation, and ordeal — the tenth-century pattern returns.
% FOUNDING_PROBLEM: Post-Carolingian fragmentation left homicide unpunished and the strong unopposed: castle-holders waged private war, church lands and peasant harvests burned in lineage conflicts, and no royal court reached most killings. The Peace of God councils (from Charroux, 989) and the Truce of God were built to shield the undefended and break the retaliation cycle; penitential discipline attacked the feud obligation itself as usurpation of God's prerogative.
% FOUNDING_PROBLEM_CORROBORATION: Royal chancery rolls and borough court records from the thirteenth century attest that crown criminal jurisdiction, not the peace apparatus, absorbed most homicide — corroborating partial obsolescence from outside the church's beneficiary seat. Saga and notarial records from Iceland (after 1262-64) and Corsica attest feud persistence where delegated enforcement failed, corroborating that the underlying problem outlived the apparatus in ungoverned zones. No source outside the benefiting parties attests that the apparatus's fee schedule tracked pastoral necessity; that attestation exists only in episcopal registers.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: substantial burdens ride on the arrangement — composition fees above customary rates, penitential tariffs, tithe funding of the enforcement apparatus, spiritual terror applied to both sides of every feud, and the loss of kin-autonomous justice — against real delivered goods (protected classes, truce windows, payment substituting for killing). Suppression 0.78 is authored as a RAW structural property, unscaled by power or scope: the regime actively suppresses its alternative (customary feud justice) through excommunication, interdict, penitential compulsion, and crown penalty; only the engine scales it. Theater_ratio 0.35: peace assemblies sometimes ceremonially ratified existing standings between lineages, and truces punctuated rather than ended feuds, but documented violence reduction in protected categories keeps the functional share dominant. Accessibility_collapse 0.45: feud options persisted at rising cost throughout the interval — wergild composition remained available, and frontier zones sat wholly outside enforcement reach — so alternatives narrowed but never collapsed. Resistance 0.60: magnates resisted peace oaths, evaded courts, and sustained covert feud for centuries. MEASUREMENT GRID: all three series share one six-point grid (989/1040/1095/1150/1200/1250) so no metric row is sampled against another's gaps. The suppression_requirement series is authored deliberately: the story's traced dynamic is enforcement-capacity build-up (penitential discipline maturing into crown criminal law), a rising trajectory, not a static picture. Base extractiveness accumulates through 1200 (rent layers on coordination) then plateaus and dips slightly by 1250 as crown courts absorb homicide jurisdiction and part of the fee flow migrates to royal seats. COALITION NOTE: the powerless peasant seat occasionally generated coalition power — sworn communal peace associations that armed collectively — which is why resistance at the class level never fell to zero even where individual exit was impossible. RECEIPT SURFACE: gains demonstrably accrue to the church seat (tariffs, fees, endowments, jurisdiction), so gain_flow names it rather than asserting diffuseness; fixing_cost is prohibitive — completing suppression required enforcement capacity no seat possessed, and dismantling the regime would forfeit the church's jurisdictional core, so either direction of repair cost more than the fixer stood to gain.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural facts. From the church and royal seats the arrangement is coordination they personally administer: they wrote the oaths, run the tribunals, and experience enforcement as cost, so their effective burden sits near the beneficiary end. From the feud_bound_kin_groups seat the same structure operates as a double bind with identity-locked exit — avenge and be damned, forbear and be dishonored — computing near the full-target end despite the lineage's organized power. The peasant seat computes mixed: protection received against tithes paid, with no exit at all. The excluded arbiter seat experiences displacement without representation. The engine derives these divergent classifications from the declared roles, exits, and scopes; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. latin_church_hierarchy: declared beneficiary with arbitrage-grade exit — d near the beneficiary end; the constraint subsidizes it (fees, endowments, jurisdictional reach). royal_jurisdiction_officials: agenda_setter with secondary beneficiary position and arbitrage exit — likewise low d, with the caveat that its enforcement costs are real and regionally variable. feud_bound_kin_groups: declared victim with identity_locked exit — d near the full-target end; identity lock amplifies effective extraction beyond what a mobile actor at the same power level would bear. wergild_debtor_households: victim, trapped — high d. peasant_communities_in_feud_zones: payer with secondary beneficiary position, trapped — mid-range d; the protection they receive damps, the tithes and unpaid protection deficits amplify. customary_feud_arbiters: excluded, contributing no beneficiary/victim derivation but marking the consensus gap. canon_law_commentators: analytical seat, no directional stake. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against pure-snare mislabeling: the coordination function is documented and real — Peace statutes named protected classes, truce windows measurably moved violence, and composition substituted for killing at scale — so the church's rents alone do not reduce the arrangement to cover-story extraction. Against pure-rope mislabeling: the asymmetry is equally documented — fees above customary rates, jurisdiction captured from kin councils, honor systems criminalized, and a beneficiary seat that certifies the settlements it taxes. The arrangement is a genuine hybrid held together by active enforcement, which is the tangled_rope structure. It is not a scaffold: no sunset clause was ever declared, and the apparatus outlived its transitional phase into permanent jurisdiction. It is not a piton during this interval: theater_ratio stays below the atrophy range, the founding problem (elite predation on the undefended) was live for most of the span, and the R5 interview returns status=contested crossed with verdict=world_rearranges — no dead-mandate-plus-dependency mismatch fires. The obsolescence signal that does exist (crown absorption of homicide jurisdiction after ~1200) is captured by the extractiveness plateau in the measurement series rather than by a premature mandate-death declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates one reading (christianized_pacification) of the feud_obligation_kernel; how would the sibling readings restructure the beneficiary and victim sets and the resulting classification?',
    'Author the sibling stories (feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading) and compare computed classifications; the disagreement is located in the legitimacy premise — whether kin vengeance is ever lawful, merely dysfunctional, or functional.',
    'Under stateless_coordination_reading, kin groups become net beneficiaries and the church empties from the beneficiary set; under extraction_cycle_reading, kin groups remain victims but no divine-law vindication attaches and the church drops out entirely. The tangled_rope classification of THIS story holds only within this reading''s frame; cross-reading epsilon comparison is invalid by construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame indexicality: one of three readings of the feud obligation kernel, with reading-dependent beneficiary/victim structure.').

omega_variable(
    pastoral_sincerity_vs_jurisdictional_rent,
    'Was the prohibition drive primarily protective (saving souls, shielding the defenseless) or primarily jurisdictional (monopolizing violence-legitimation and the fees attached to it)?',
    'Compare enforcement expenditure against fee revenue across sees; identify prosecutions pursued at net fiscal loss; trace whether enforcement intensity tracks danger to the helpless or revenue density of the district.',
    'Primarily protective pushes the arrangement toward rope with incidental rents; primarily rential pushes it toward snare wearing liturgical dress. Current authoring assumes a genuine mix, which is what sustains the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pastoral_sincerity_vs_jurisdictional_rent, empirical, 'Motivation mix behind the pacification apparatus: protection versus rent.').

omega_variable(
    honor_identity_vs_enforcement_gap,
    'Does feud practice persisting under prohibition reflect internalized honor identity or merely enforcement incapacity?',
    'Compare feud persistence in regions where crown criminal law reliably punished homicide against regions where it did not; persistence despite reliable punishment indicates the obligation was carried internally by the lineages rather than imposed externally.',
    'If internalized, the suppression measure understates the true binding — the obligation travels with the lineage even where courts withdraw — and effective suppression exceeds the structural 0.78; if structural, completing enforcement completes pacification and the residual feud is an enforcement-budget artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_identity_vs_enforcement_gap, empirical, 'Structural versus internalized binding of the feud obligation under prohibition.').

omega_variable(
    peace_assembly_functionality,
    'Did peace assemblies and truce windows reduce actual violence, or theatrically ratify existing standings between lineages?',
    'Match charter-recorded truce windows and peace-oath enrollments against independent violence indicators (homicide entries, harrying records, exile counts) inside versus outside protected windows and protected classes.',
    'If largely theatrical, theater_ratio is understated and the arrangement drifts toward inertial maintenance earlier than the series shows; if functional, the authored 0.35 stands and the coordination-function gate stays satisfied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peace_assembly_functionality, empirical, 'Functional versus ceremonial content of the peace legislation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 989, 1250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_pac_tr_t989, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 989, 0.18).
narrative_ontology:measurement_basis(feud_pac_tr_t989, observed).
narrative_ontology:measurement(feud_pac_tr_t1040, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1040, 0.22).
narrative_ontology:measurement_basis(feud_pac_tr_t1040, observed).
narrative_ontology:measurement(feud_pac_tr_t1095, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1095, 0.27).
narrative_ontology:measurement_basis(feud_pac_tr_t1095, observed).
narrative_ontology:measurement(feud_pac_tr_t1150, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1150, 0.31).
narrative_ontology:measurement_basis(feud_pac_tr_t1150, observed).
narrative_ontology:measurement(feud_pac_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.34).
narrative_ontology:measurement_basis(feud_pac_tr_t1200, observed).
narrative_ontology:measurement(feud_pac_tr_t1250, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1250, 0.35).
narrative_ontology:measurement_basis(feud_pac_tr_t1250, observed).

% Extraction over time
narrative_ontology:measurement(feud_pac_be_t989, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 989, 0.44).
narrative_ontology:measurement_basis(feud_pac_be_t989, observed).
narrative_ontology:measurement(feud_pac_be_t1040, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1040, 0.51).
narrative_ontology:measurement_basis(feud_pac_be_t1040, observed).
narrative_ontology:measurement(feud_pac_be_t1095, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1095, 0.57).
narrative_ontology:measurement_basis(feud_pac_be_t1095, observed).
narrative_ontology:measurement(feud_pac_be_t1150, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1150, 0.62).
narrative_ontology:measurement_basis(feud_pac_be_t1150, observed).
narrative_ontology:measurement(feud_pac_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.64).
narrative_ontology:measurement_basis(feud_pac_be_t1200, observed).
narrative_ontology:measurement(feud_pac_be_t1250, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1250, 0.62).
narrative_ontology:measurement_basis(feud_pac_be_t1250, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_pac_su_t989, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 989, 0.4).
narrative_ontology:measurement_basis(feud_pac_su_t989, observed).
narrative_ontology:measurement(feud_pac_su_t1040, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1040, 0.52).
narrative_ontology:measurement_basis(feud_pac_su_t1040, observed).
narrative_ontology:measurement(feud_pac_su_t1095, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1095, 0.61).
narrative_ontology:measurement_basis(feud_pac_su_t1095, observed).
narrative_ontology:measurement(feud_pac_su_t1150, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1150, 0.7).
narrative_ontology:measurement_basis(feud_pac_su_t1150, observed).
narrative_ontology:measurement(feud_pac_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.75).
narrative_ontology:measurement_basis(feud_pac_su_t1200, observed).
narrative_ontology:measurement(feud_pac_su_t1250, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1250, 0.78).
narrative_ontology:measurement_basis(feud_pac_su_t1250, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'blood feud' decomposes into three structurally distinct constraints sharing the feud_obligation_kernel, per the epsilon-invariance principle. (1) This file, christianized_pacification_reading: the prohibition-and-delegation regime — tangled_rope, church and royal beneficiaries, all feud participants bearing spiritual and material burden, epsilon 0.62. (2) stateless_coordination_reading: the feud obligation as self-enforcing coordination providing justice and deterrence absent central capacity — rope-flavored, kin groups as beneficiaries, epsilon far lower. (3) extraction_cycle_reading: the feud obligation as a destructive extraction loop — snare-flavored, kin groups as victims, no church beneficiary, no vindicated doctrine. The epsilons differ because the referent arrangements differ; forcing one story to carry all three would make epsilon observable-dependent. Causal ordering across the family: the stateless-coordination account explains why feud emerged (upstream condition), and this reading's prohibition apparatus supplied the enforcement coalition that acted on the extraction-cycle diagnosis (downstream pressure). Each sibling file links back through its own network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
