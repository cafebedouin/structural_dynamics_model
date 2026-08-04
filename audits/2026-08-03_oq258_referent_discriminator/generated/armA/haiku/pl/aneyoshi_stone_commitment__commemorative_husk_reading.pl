% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone: Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/institutional_decay
 *
 * SUMMARY:
 *   The Aneyoshi stone, placed in the 1600s–1800s with an injunction to build
 *   above a marked elevation to avoid tsunami death, functioned as a live
 *   behavioral constraint for approximately 78 years. By the late 20th
 *   century, it had become a heritage artifact and memorial marker. The 2011
 *   Tōhoku tsunami tested both the stone's directive force and the
 *   community's adherence to it: Aneyoshi experienced zero deaths, but
 *   post-tsunami analysis attributes survival to modern building codes,
 *   early-warning systems, and evacuation procedures—not to compliance with a
 *   centuries-old stone marker. This constraint story instantiates the
 *   'commemorative husk' reading: the stone persists as a symbolic object
 *   that absorbs cultural labor and tourism value, while its original
 *   prescriptive force has atrophied. The claim/metric gap is deliberate and
 *   central to the reading: theater_ratio rises from 0.45 to 0.87 as
 *   functional governance decays and performative maintenance increases;
 *   extractiveness stays low because no party systematically collects rents
 *   from the stone's memorial status—only diffuse cultural identity and
 *   tourism spillover accrue. The sibling reading
 *   (behavioral_competence_reading) would claim the stone's directive
 *   remained operationally alive and explains 2011 survival; this reading
 *   claims the stone's survival is explained by institutional/geological luck
 *   while the commitment's binding force is gone.
 *
 * KEY AGENTS:
 *   - aneyoshi_community: inhabitant seat; maintains the stone as memorial but does not obey its directive
 *   - cultural_preservation_advocates: agenda-setter seat; advocates for stone's conservation and heritage status
 *   - municipal_land_planners: agenda-setter seat; treat the stone as heritage attraction, not land-use rule
 *   - tourism_economy: beneficiary seat; collects economic value from stone's memorial status
 *   - historians_and_interpreters: observer seat; document the divergence between prescriptive intent and commemorative function
 *   - disaster_risk_management_authorities: excluded seat; would reactivate the stone as policy input but are structurally absent from its contemporary governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.87).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.87).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone: Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/institutional_decay").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'eb28a1e4-db4d-4a95-8ef9-010471f61401').
narrative_ontology:cs_kernel_codification('eb28a1e4-db4d-4a95-8ef9-010471f61401', fixed_text).
narrative_ontology:cs_authority_grounding('eb28a1e4-db4d-4a95-8ef9-010471f61401', lineage).
narrative_ontology:cs_interpretation_layer_present('eb28a1e4-db4d-4a95-8ef9-010471f61401').
narrative_ontology:cs_reading_relation('eb28a1e4-db4d-4a95-8ef9-010471f61401', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('eb28a1e4-db4d-4a95-8ef9-010471f61401', foundational, stone_functions_as_memorial_symbol).
narrative_ontology:cs_axiom_status(stone_functions_as_memorial_symbol, holdable).
narrative_ontology:cs_axiom_grounding('eb28a1e4-db4d-4a95-8ef9-010471f61401', stone_functions_as_memorial_symbol, deontological).
narrative_ontology:cs_axiom('eb28a1e4-db4d-4a95-8ef9-010471f61401', foundational, behavioral_compliance_decayed_pre_2011).
narrative_ontology:cs_axiom_status(behavioral_compliance_decayed_pre_2011, holdable).
narrative_ontology:cs_axiom_grounding('eb28a1e4-db4d-4a95-8ef9-010471f61401', behavioral_compliance_decayed_pre_2011, empirically_contingent).
narrative_ontology:cs_reference_frame('eb28a1e4-db4d-4a95-8ef9-010471f61401', heritage_memorial_function).
narrative_ontology:cs_drift_state('eb28a1e4-db4d-4a95-8ef9-010471f61401', contemporary_post_2011_institutional_context, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('eb28a1e4-db4d-4a95-8ef9-010471f61401', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, cultural_preservation_advocates).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_community).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_community).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, historical_memory_preservation).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, symbolic_continuity_after_disaster).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the stone as a memorial and place-marker of collective disaster experience. The stone structures their identity as 'the village that survived tsunamis' but does not constrain their land-use decisions—new buildings and infrastructure are sited based on economic need, municipal planning, and convenience, not the stone's injunction. The stone persists because it holds memorial meaning; behavioral compliance is absent.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_community, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_community, payer).

% Advocate for the stone's conservation and interpretation as historical artifact. They frame it as evidence of collective memory and ancestral wisdom, justify its upkeep and signage through heritage narratives, and resist its removal or relegation to museum storage. The stone is maintained theatrically—as a monument to a lost commitment—rather than functionally as a behavioral rule.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, cultural_preservation_advocates, agenda_setter,
    organized, generational, mobile, national).

% Make land-use and building-location decisions independently of the stone's directive. They are aware of it as a historical artifact and tourist marker but treat it as a heritage consideration for signage and viewsheds, not as a binding constraint on construction height, setback, or location. The 2011 tsunami survival was attributed post-facto to geological chance and building codes, not to adherence to the stone's 78-year-old directive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_land_planners, agenda_setter,
    institutional, biographical, analytical, local).

% Collects value from the stone as a heritage attraction: guided tours, interpretive signage, disaster-tourism packages, and media coverage. The stone's economic yield is proportional to its status as a historical curiosity and symbol of collective memory, not to its functional governance of land use. If the stone disappeared, heritage tourism would relocate; if the directive were actually enforced, land use would shift but tourism appeal might diminish.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_economy, beneficiary,
    organized, biographical, mobile, regional).

% Analyze the stone's role in collective memory and institutional decay. They document the divergence between the stone's original prescriptive intent (behavioral constraint on settlement) and its contemporary function (memorial artifact). They track the rhetorical shift from 'obey this rule' to 'remember what our ancestors knew' and assess whether the 2011 survival validates the stone's directive or coincides with it by chance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, historians_and_interpreters, observer,
    analytical, generational, analytical, national).

% Are excluded from shaping how the stone is treated as a governance tool. They could use it to inform building codes and evacuation planning, but it functions as heritage rather than policy input. They attest (from post-2011 analysis) that the directive's protective value lay in its 78-year force, not in the stone itself—when behavioral compliance lapsed, the stone's ability to constrain settlement atrophied, leaving only commemorative function.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_risk_management_authorities, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Collective memory of multi-generational disaster knowledge: the stone encodes and transmits the ancestral warning that settlement above a certain elevation prevents tsunami death. In this reading, coordination has atrophied—the knowledge is remembered emotionally and culturally, not operationalized in land-use decisions.
% TRANSFER_FUNCTION: Moves attention and cultural capital from present-day planners and residents to the historical narrative. The stone absorbs maintenance labor (cleaning, interpretation signage, heritage conservation) and redirects economic value toward tourism and cultural identity, away from enforceable settlement rules.
% ABSENT_VOICES: Future disaster-risk planners, alternative settlement communities, and the stone's original authors (deceased 78+ years) are structurally excluded from the contemporary interpretation. The sibling reading (behavioral_competence_reading) would argue forcefully that the stone's prescriptive force should animate current planning; this reading excludes that claim by treating the stone as a historical artifact, not a policy instrument.
% DISAPPEARANCE_RATIONALE: If the stone were removed, the commemorative function would relocate to a museum or memorial marker, but the community's identity as 'tsunami-wise' would persist. Land-use decisions would be unaffected because they already ignore the stone's directive. The behavioral-competence reading would claim removal proves the community has lost its ancestral wisdom and become vulnerable; the commemorative reading claims the stone's disappearance would be a loss of cultural symbol but not operational constraint. The world's arrangement depends on the stone's heritage status, not its governing force.
% FOUNDING_PROBLEM: In the 1600s–1800s, multiple tsunamis killed settlers who built in low-lying areas. Aneyoshi elders placed a stone at the elevation above which settlement was safe, with an injunction: build above this mark or perish. The stone's founding problem was existential: how to encode and preserve multi-generational disaster knowledge in a community that forgets.
% FOUNDING_PROBLEM_CORROBORATION: Geologists and disaster-risk specialists attest the founding problem (preserving knowledge across generational amnesia) was solved for ~78 years—the stone's directive constrained settlement, and no tsunami deaths occurred in Aneyoshi during that period. Historians and the 2011 post-tsunami investigations attest that by 2011, the founding problem's solution had decayed: the stone was known as a heritage artifact but not obeyed as a rule. The survival of Aneyoshi in 2011 is attributed by seismic engineers and municipal records to improved building codes, early-warning systems, and evacuation procedures—not to adherence to the stone's 400-year-old directive. The commitment's functional force is gone; only its commemorative shell remains.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).
:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is high (0.87 at interval end) because the stone's contemporary function is almost entirely performative: signage, guided tours, heritage conservation, identity affirmation. The functional activity (behavioral constraint on settlement) has atrophied to near-zero. Extractiveness is very low (0.12) because no agent systematically extracts surplus from the stone's operation—cultural preservation advocates maintain it at modest cost, tourism economy collects spillover value, the community gains identity without bearing constraint. Suppression is minimal (0.08) because no coercive enforcement exists; the stone's injunction is voluntarily ignored, not suppressed. Accessibility_collapse is low (0.22) because alternatives (modern building codes, evacuation procedures) are readily available and openly adopted; the stone's prohibition has collapsed to near-zero practical force. Resistance is low (0.15) because few agents actively contest the commemorative reading; the behavioral-competence reading exists but lacks institutional backing post-2011. The measurement series model the constraint's lifecycle: functional force (extractiveness ~0.03–0.05 early) declining as theater rises (~0.45 to 0.87) and the commitment kernel migrates from operational rule to heritage symbol. The interval spans approximately 78 years of the stone's post-placement history through 2011 and beyond.
 *
 * PERSPECTIVAL GAP:
 *   The community's perspective: the stone is a cherished memorial of ancestral wisdom and collective identity, its survival validates cultural continuity. The behavioral-competence reading's perspective: the stone is a failed governance instrument whose decline explains why 2011 survival required external systems (building codes) to compensate. The historians' perspective: the stone exemplifies institutional commitment decay—the kernel (ancestral warning) was operationalized in stone form, but when behavioral compliance lapsed and alternative systems emerged, the stone migrated to pure symbol. These perspectives diverge sharply on what explains the stone's continued existence: cultural meaning vs. functional obsolescence.
 *
 * DIRECTIONALITY LOGIC:
 *   The community_aneyoshi is a dual-positioned agent: they are the beneficiary of cultural identity and memorial meaning (low d), while also the payer of maintenance labor and identity-fusion costs (moderate d—their sense of place is bound to the stone's narrative). Cultural_preservation_advocates benefit from the stone's heritage status and conservation work (d ~0.2, near-beneficiary). Municipal_land_planners and disaster_risk_management are observers and excluded seats; they bear no extraction or benefit from the stone's memorial function directly. Tourism_economy is a pure beneficiary (d ~0.1, low—collects spillover without burden). The directionality profile reflects the constraint's piton character: costs are diffuse (community identity labor, conservation funding) and benefits are diffuse (cultural meaning, tourism). No single agent extracts concentrated rents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint models mandatrophy—the decay of a commitment's original mandate while the commitment persists through theatrical maintenance. The founding problem (preserving tsunami-avoidance knowledge) was solved for ~78 years via the stone's prescriptive force. By 2011, the problem persisted at a civilizational level (multi-generational knowledge transmission) but was solved via alternative mechanisms (education, building codes, early-warning systems). The stone's mandate expired—behavioral constraint became unnecessary—but the commitment persisted as a cultural symbol. The engine would compute this as piton: low extraction, high theater, no concentrated beneficiary bearing the cost, diffuse memorial function maintained by institutional inertia (heritage designation, tourism interest, cultural identity). The classification prevents mischaracterization as 'rope' (genuine coordination) or 'snare' (systematic extraction); it identifies the structure as performative persistence of a commitment whose functional purpose has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_2011_survival,
    'Did Aneyoshi''s zero tsunami deaths in 2011 result from behavioral compliance with the stone''s elevation directive, or from modern building codes, early-warning systems, and evacuation procedures that would have protected the village regardless of the stone?',
    'Comparative analysis of other communities that (a) ignored the stone-equivalent directive but adopted modern protective systems, or (b) obeyed traditional rules but lacked modern systems. Engineering post-mortems and evacuation records from 2011 can isolate the causal contributions of each factor.',
    'If modern systems are causally sufficient for 2011 survival, the stone''s operational force is nil and the commemmorative reading holds; if behavioral compliance with the directive is necessary or substantially contributory, the behavioral-competence reading gains empirical support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_attribution_2011_survival, empirical, 'The attribution of 2011 survival to stone-compliance vs. institutional factors.').

omega_variable(
    kernel_stability_under_mandatrophy,
    'Is the aneyoshi_stone_commitment a single kernel with two readings of its contemporary status, or are the behavioral and commemorative interpretations so structurally distinct that they represent two separate commitments?',
    'Textual and genealogical analysis of the stone''s inscriptions, the oral traditions surrounding it, and the institutional practices (land-use vs. heritage conservation) that cite it. If both readings claim fidelity to the same ancestral injunction while disagreeing on its binding force, the kernel is one with divergent readings; if one reading treats the stone as foundational (behavioral) and the other as post-hoc reinterpretation (commemorative), the commitments may be distinct.',
    'If the commitment is one kernel with two readings, the piton classification (low extraction, high theater) is correct for the commemorative reading while the behavioral reading would classify higher in prescriptive force. If they are distinct commitments, the network relationship is a constraint family, not kernel siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_stability_under_mandatrophy, conceptual, 'Whether the behavioral and commemorative stances describe one kernel with divergent interpretations or two separate commitment structures.').

omega_variable(
    theater_ratio_interpretation_in_memorial_context,
    'Does the high theater_ratio (0.87) in the commemorative reading accurately capture the constraint''s operation, or does it impose a category (performance vs. function) that is incoherent for memorial and cultural artifacts?',
    'Philosophical analysis of what counts as ''functional'' in a cultural-memorial context. A memorial''s ''function'' is to carry and transmit meaning, not to constrain behavior; the theater/function distinction may be category-mismatch for artifacts whose primary purpose is symbolic. Alternatively, apply the theater ratio to cultural constraints generally and establish whether it discriminates piton-class cases (atrophied governance structures) from intentional memorials.',
    'If the memorial function is genuine and primary (meaning-transmission, identity affirmation), the theater_ratio may misclassify the constraint as piton (degraded) when it is actually a rope or scaffold in cultural work. If theater_ratio correctly identifies that behavioral governance has atrophied, the piton classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratio_interpretation_in_memorial_context, conceptual, 'Whether the performance/function distinction applies coherently to memorial and cultural artifacts.').

omega_variable(
    reading_distinction_foreclosure_test,
    'Can a single party (the community, planners, historians, preservation advocates) coherently hold BOTH the behavioral-competence reading AND the commemorative-husk reading simultaneously, or does the core premise of each reading logically foreclose the other?',
    'Textual analysis of actual party statements and institutional positions post-2011. If Aneyoshi residents, municipal planners, or heritage advocates hold both readings in tension—''the stone is a living rule AND a memorial symbol''—then the readings coexist. If the behavioral claim (''the directive constrained settlement for 78 years'') and the memorial claim (''the stone is a heritage artifact, not a binding rule'') are treated as mutually exclusive, then one reading forecloses the other.',
    'If coexistence is actual practice, reading_relations should be ''coexists_with''. If the readings are treated as mutually exclusive, reading_relations should be ''forecloses'' (rare) or ''influences'' (the commemorative reading de-legitimizes the behavioral claim by reframing it as nostalgia). The engine computes foreclosure from axiom_contradiction; the omega documents whether the parties treat the readings as exclusive or in creative tension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinction_foreclosure_test, empirical, 'Whether the behavioral and commemorative readings coexist or logically foreclose each other in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(aney_tr_t0, projected).
narrative_ontology:measurement(aney_tr_t10, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(aney_tr_t10, projected).
narrative_ontology:measurement(aney_tr_t20, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 20, 0.71).
narrative_ontology:measurement_basis(aney_tr_t20, observed).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 30, 0.78).
narrative_ontology:measurement_basis(aney_tr_t30, observed).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 45, 0.86).
narrative_ontology:measurement_basis(aney_tr_t45, observed).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 60, 0.87).
narrative_ontology:measurement_basis(aney_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(aney_be_t0, projected).
narrative_ontology:measurement(aney_be_t10, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement_basis(aney_be_t10, projected).
narrative_ontology:measurement(aney_be_t20, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement_basis(aney_be_t20, observed).
narrative_ontology:measurement(aney_be_t30, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement_basis(aney_be_t30, observed).
narrative_ontology:measurement(aney_be_t45, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 45, 0.12).
narrative_ontology:measurement_basis(aney_be_t45, observed).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement_basis(aney_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(aney_su_t0, projected).
narrative_ontology:measurement(aney_su_t10, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 10, 0.04).
narrative_ontology:measurement_basis(aney_su_t10, projected).
narrative_ontology:measurement(aney_su_t20, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 20, 0.06).
narrative_ontology:measurement_basis(aney_su_t20, observed).
narrative_ontology:measurement(aney_su_t30, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 30, 0.07).
narrative_ontology:measurement_basis(aney_su_t30, observed).
narrative_ontology:measurement(aney_su_t45, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 45, 0.08).
narrative_ontology:measurement_basis(aney_su_t45, observed).
narrative_ontology:measurement(aney_su_t60, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 60, 0.08).
narrative_ontology:measurement_basis(aney_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, attachment_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone commitment is instantiated as two constraint stories representing contested kernel readings: (1) behavioral_competence_reading claims the stone's directive retained operational force in settlement decisions across 78 years and explains 2011 survival through behavioral compliance; (2) commemorative_husk_reading (this story) claims the directive's prescriptive force atrophied while the stone persisted as a memorial and heritage artifact, and attributes 2011 survival to institutional/geological factors. The two readings diverge on the same kernel's contemporary status. epsilon values differ substantially (behavioral reading: higher extractiveness from settlement constraint; commemorative reading: near-zero behavioral extraction, high theater). Each reading declares its own stabilized commitment structure, beneficiary/victim set, and classification. They are linked as kernel siblings via reading_relations (coexists_with or influences, per the resolution of Omega #4).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
