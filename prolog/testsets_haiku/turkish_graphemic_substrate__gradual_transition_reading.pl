% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Turkish Gradual Graphemic Transition (Dual-Script Coexistence)
 *   domain: political/cultural/linguistic
 *
 * SUMMARY:
 *   Turkey's 1928 script reform replaced Ottoman Arabic script with Latin
 *   script to align with European modernization and distance Turkish identity
 *   from Islamic civilization. This reading instantiates ONE interpretation
 *   of how to manage the intergenerational consequences: permitting both
 *   scripts to coexist in defined institutions (religious schools, archives,
 *   heritage programs) for 5-15 years while gradually shifting the primary
 *   education system to Latin-only. This is NOT the Ottoman continuity
 *   reading (which argues Arabic script should remain legitimate
 *   indefinitely) nor the secular nationalist reading (which argues the 1928
 *   reform was correct and immediate Latin-only education should have
 *   followed without transition). This reading claims a scaffold: temporary
 *   coexistence, justified by the coordination problem of avoiding rupture,
 *   with an embedded sunset clause that commits to eventual Latin primacy.
 *   The constraint's structure encodes a decision about the *rate* and *path*
 *   of state homogenization—slower and more managed than the 1928
 *   instantaneous shift, but still moving toward Latin-monolith outcome.
 *
 * KEY AGENTS:
 *   - State language authority: Sets the dual-script policy, administers enforcement, controls the sunset timing
 *   - Elder literacy preservers (elderly Arabic-script readers, religious scholars, archive custodians): Benefit from continued Arabic legitimacy but depend on state recognition of their institutions
 *   - Intergenerational bridge institutions (religious schools, cultural heritage bodies): Bear the cost of teaching both scripts while benefiting from dual authorization
 *   - Primary education cohorts (students during the 15-year transition): Forced to achieve dual-script competency despite resources flowing toward Latin
 *   - Rapid modernization advocates (business, European-integration technocrats): Bear the cost of delayed full Latin consolidation; see dual-script education as resource waste
 *   - Arabic-script monolingual elderly: Trapped in inability to access Latin-script services; excluded from the decision process
 *   - Ottoman continuity advocates: Structurally excluded; the sunset clause forecloses their preferred outcome within this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.58).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.72).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Turkish Gradual Graphemic Transition (Dual-Script Coexistence)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political/cultural/linguistic").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '6549bd3d-e200-4d34-bdcd-023a2dab9af7').
narrative_ontology:cs_kernel_codification('6549bd3d-e200-4d34-bdcd-023a2dab9af7', fixed_text).
narrative_ontology:cs_authority_grounding('6549bd3d-e200-4d34-bdcd-023a2dab9af7', extraction).
narrative_ontology:cs_interpretation_layer_present('6549bd3d-e200-4d34-bdcd-023a2dab9af7').
narrative_ontology:cs_reading_relation('6549bd3d-e200-4d34-bdcd-023a2dab9af7', turkish_graphemic_substrate__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6549bd3d-e200-4d34-bdcd-023a2dab9af7', turkish_graphemic_substrate__secular_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('6549bd3d-e200-4d34-bdcd-023a2dab9af7', foundational, dual_script_coexistence_temporally_justified).
narrative_ontology:cs_axiom_status(dual_script_coexistence_temporally_justified, holdable).
narrative_ontology:cs_axiom_grounding('6549bd3d-e200-4d34-bdcd-023a2dab9af7', dual_script_coexistence_temporally_justified, instrumental).
narrative_ontology:cs_axiom('6549bd3d-e200-4d34-bdcd-023a2dab9af7', foundational, intergenerational_knowledge_transfer_requires_transition_period).
narrative_ontology:cs_axiom_status(intergenerational_knowledge_transfer_requires_transition_period, holdable).
narrative_ontology:cs_axiom_grounding('6549bd3d-e200-4d34-bdcd-023a2dab9af7', intergenerational_knowledge_transfer_requires_transition_period, empirically_contingent).
narrative_ontology:cs_axiom('6549bd3d-e200-4d34-bdcd-023a2dab9af7', secondary, latin_script_eventual_primacy_inevitable).
narrative_ontology:cs_axiom_status(latin_script_eventual_primacy_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('6549bd3d-e200-4d34-bdcd-023a2dab9af7', latin_script_eventual_primacy_inevitable, instrumental).
narrative_ontology:cs_reference_frame('6549bd3d-e200-4d34-bdcd-023a2dab9af7', ottoman_arabic_script_legitimacy).
narrative_ontology:cs_drift_state('6549bd3d-e200-4d34-bdcd-023a2dab9af7', contemporary_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6549bd3d-e200-4d34-bdcd-023a2dab9af7', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, elder_literacy_preservers).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_bridge_institutions).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, state_curriculum_architects).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, arabic_script_monolingual_elderly).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, latin_script_primary_education_cohorts).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, rapid_modernization_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_bridge_institutions).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, primary_education_cohorts).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, linguistic_continuity_reconcilable_with_modernization).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, managed_state_script_transitions_reduce_rupture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the dual-script policy, administers enforcement, controls the sunset timing. Justifies the policy as minimizing generational rupture while enabling modernization. Collects legitimacy from international education bodies and moderate elites for 'managed' approach. Faces pressure from rapid modernization advocates to accelerate the sunset and from Ottoman continuity advocates to extend it indefinitely.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_language_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Custodians of Ottoman-era texts, religious scholars, archive administrators. The dual-script policy permits continued use of Arabic script in their institutions for defined purposes, averting the abrupt linguistic displacement that immediate Latin-only would impose. They benefit from continued institutional legitimacy of their literacy and teaching roles but are constrained by the sunset clause, which guarantees eventual deprioritization of Arabic.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, elder_literacy_preservers, beneficiary,
    organized, biographical, constrained, national).

% Religious schools, cultural heritage organizations, archive-keeping institutions. Benefit from dual-script authorization and can maintain intergenerational knowledge transfer. Pay higher operational costs (dual-script curriculum development, teacher training in both scripts, textbook production in both). Their institutional identity depends on intergenerational knowledge transfer; the managed transition permits it but at sustained overhead until the sunset.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_bridge_institutions, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_bridge_institutions, payer).

% Students during the transition period must achieve minimum competency in BOTH scripts despite primary curriculum emphasizing Latin. Bear cognitive and time cost of dual-script instruction without heritage motivation (Arabic is not their native literacy). Identity is locked to state school attendance and curriculum mandates; exit alternatives (private non-dual schools) exist but are constrained by state accreditation policy requiring dual-script proof for certain credentials.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, primary_education_cohorts, payer,
    moderate, biographical, identity_locked, national).

% Technocrats, business leaders, European integration advocates. Argue dual-script education diverts resources from Latin mastery and slows international integration. View the transition policy as an expensive residue of sentiment about the 1928 reform. Must operate within the constrained policy framework and cannot immediately mandate Latin-exclusive education, though they do have influence over the sunset-acceleration debate.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, rapid_modernization_advocates, payer,
    powerful, generational, constrained, national).

% Populations with no Latin-script literacy, dependent on Arabic-script texts (religious materials, legal documents, family records). The policy permits their continued reliance on Arabic in defined domains but traps them in inability to access the expanding Latin-script state apparatus. Excluded from curriculum planning conversations that shape the transition; their testimony about the hardship of 1928 rupture is occasionally cited but not acted upon.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, arabic_script_monolingual_elderly, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, arabic_script_monolingual_elderly, excluded).

% Intellectuals, religious scholars, cultural traditionalists arguing Arabic script is the legitimate substrate of Turkish identity. Would advocate for Arabic-script primacy or permanent dual-script official status. Structurally excluded from the transition decision (the sunset clause forecloses their preferred outcome). See the temporary coexistence as a concession that will inevitably disappear, converting the 15-year reprieve into a slow-motion loss.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ottoman_continuity_advocates, excluded,
    moderate, generational, constrained, national).

% UNESCO, EU education programs, comparative literacy researchers. Monitor the transition's outcomes (literacy rates, intergenerational knowledge transfer, educational equity). Provide external assessment of whether dual-script phase achieved its purpose. Inform (but do not set) decisions about consolidation or continuation past sunset.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, international_education_bodies, observer,
    institutional, generational, analytical, global).

% Education ministry planners and pedagogical experts designing the dual-script curriculum and its phasing. Benefit from the policy's framing as 'managed transition' (appears evidence-based, humane, and professionally sophisticated). Also enforce the Latin-primary trajectory embedded in the sunset clause through curriculum standards and teacher training requirements. Institutional legitimacy depends on demonstrating the transition is necessary while being managed compassionately.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_curriculum_architects, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, state_curriculum_architects, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__gradual_transition_reading, state_language_authority).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__gradual_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective problem of script discontinuity in state literacy: how to shift an entire educated population from Arabic to Latin script without creating a rupture in intergenerational knowledge transmission, institutional continuity (especially religious and archival institutions), and social cohesion. A unilateral immediate Latin-only shift would sever access to Ottoman literary, legal, and religious heritage; isolate elderly from young; and destabilize institutions built on Arabic-script authority. The coordination problem is real: without managed transition, the state faces institutional fragmentation (two literacy cohorts) and loss of historical archives.
% TRANSFER_FUNCTION: Moves educational resources (teacher training time, curriculum development, institutional duplication, textbook publishing in both scripts) from modernization acceleration into intergenerational bridge-maintenance during the 5-15 year transition window. Also defers the consolidation of state linguistic authority (shifting from Arabic-substrate Ottoman state to Latin-substrate modern nation-state) by that period, postponing the complete erasure of Arabic script's institutional legitimacy and the exclusion of elderly from state literacy spaces.
% ABSENT_VOICES: Ottoman continuity advocates are structurally excluded from the transition design: the sunset clause encodes the Latin-primary direction into the framework itself, making their advocacy for permanent dual-script or Arabic-primary status impossible to raise as a live option. Arabic-script monolingual elderly populations lack the literacy to access curriculum planning documents and lack institutional representation in education policy. Alternative script-innovation advocates (proposing intermediate phonetic systems or reformed Arabic scripts) are absent from technical discussions. Younger Latin-native speakers who would argue dual-script education is wasted effort on a solved problem are present as 'rapid modernization advocates' but less empowered than the agenda-setter.
% DISAPPEARANCE_RATIONALE: If the dual-script coexistence policy vanished and immediate Latin-only education was mandated, the state would face rapid institutional destabilization: religious institutions dependent on Arabic-script authority would lose legitimacy overnight, elderly populations would be cut off from intergenerational knowledge transfer, and the archive preservation function would require crash-course Latin training or specialist hiring. Conversely, if the policy persisted indefinitely without a sunset clause, the state would fragment permanently into two literacy cohorts (Arabic-primary elders, Latin-primary youth) with reduced linguistic unity and permanently split institutional capacity. The policy's existence is predicated on its temporality: it arranges the state only under the condition that it ends, moving it toward Latin-monolith consolidation while deferring the institutional shock.
% FOUNDING_PROBLEM: The 1928 Turkish script reform replaced Ottoman Arabic script with Latin script to align Turkey with European modernity and distance Turkish identity from Islamic civilization. This created an immediate rupture: teachers trained in Arabic had to teach Latin-only children; elderly populations were cut off from new education; intergenerational transmission of Ottoman texts, religious scholarship, and archival knowledge was interrupted. By the 2010s, as digitization and heritage preservation projects began, the institutional damage became visible: trained Arabic-script custodians had no successors, archives were becoming illegible to younger scholars, and religious institutions faced gaps in transmission. The founding problem (intergenerational rupture from script change) was discovered to be *still live* decades after the 1928 reform, not solved by time alone. A managed transition seeks to prevent this rupture from repeating during the final consolidation to Latin-only.
% FOUNDING_PROBLEM_CORROBORATION: The state language authority and curriculum architects attest the founding problem is live and requires managed correction. Educational researchers and international bodies (UNESCO comparative literacy studies, EU education assessments) corroborate that literacy transitions without explicit continuity planning produce measurable educational disruption and cultural institution attrition. Elderly Arabic-script literate populations offer testimony (outside the benefiting parties) about the hardship of the 1928 rupture and affirm that a gradual transition *would have* mitigated their experience. Ottoman continuity advocates contest the diagnosis: they argue the problem is not rupture *within* the transition, but the fact of the transition itself—that Latin script was imposed without permission, and a managed version will merely extend the injustice rather than undo it. Rapid modernization advocates contest whether the founding problem is real: they argue the 1928 reform was correct and the transition policy wastes resources on a problem that time and institutional adaptation have already solved.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high at the original 1928 reform (0.85, unilateral state script replacement with no coordination function stated—pure imposition) and decays over the transition period as the managed coexistence framework explicitly coordinates intergenerational continuity (down to 0.58 at t=2025, the current study point, and declining toward 0.41 as the sunset approaches). Theater ratio follows the same trajectory but steeper: at 1928 the new script was entirely theater (no functioning Latin education infrastructure, pure state spectacle of modernity); by 2025 the dual-script compromise is genuinely functional in heritage institutions (lower theater). Suppression is highest at 1928 (0.88, the original reform suppressed Arabic entirely) and decays as the managed transition permits Arabic in defined domains (down to 0.72 by 2025). By 2043 (end of the sunset period), suppression has fallen to 0.38 because the transition's entire point is to reduce coercive force by the time Latin becomes sole-primary. Accessibility collapse is moderate (0.65): Latin literacy now dominates institutions so thoroughly that choosing Arabic-only is structurally inaccessible, yet the explicit dual-script policy prevents the complete collapse it would face under unilateral Latin enforcement. Resistance is also moderate (0.68): Ottoman continuity advocates and elderly monoliths resist the Latin trajectory, but the scaffold's framing as 'temporary' and 'managed' dampens visible opposition relative to the 1928 reform's resistance. All measurements share one time grid covering the full 115-year lifecycle from reform to sunset-clause terminus.
 *
 * PERSPECTIVAL GAP:
 *   The foundational perspectival gap is between the authority's reading of the constraint as temporal/managed and the target seats' reading of it as a vehicle for delayed homogenization. The policy's use of the word 'transition' and the explicit sunset clause are performative—they signal good faith management to international bodies and educated elites, but they encode an irreversible direction (Latin-primacy) that the 'managed' framing obscures. To intergenerational bridge institutions and elderly populations, the policy is a 15-year reprieve, not a reversal. To rapid modernization advocates, it is an expensive holding pattern. The engine's per-seat type computation makes this gap visible by showing different seats computing to different types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The state language authority is a beneficiary (controls the policy, justifies modernization, defers political consolidation costs) with d near 0.2 (benefits substantially, mobile alternatives to their administrative framework are constrained but exist). Elder preservers are symmetric-to-target (they benefit from Arabic legitimation but are ultimately bound to the Latin-primary trajectory; d~0.55). Bridge institutions are symmetric (benefit from dual authorization but pay in operational cost; d~0.50). Primary education cohorts are high-target (forced dual literacy; d~0.75). Rapid modernization advocates are moderate-targets (constrained by the delayed full Latin shift; d~0.62). Elderly monoliths are severe targets (trapped; d~0.88). Ottoman continuity advocates are moderate-targets (the sunset clause encodes their loss; d~0.70). This directionality profile explains why the same constraint computes as scaffold from the authority's seat (low extraction, managed coordination) but as tangled_rope or snare from target seats (coordinated on paper, extractive in operation, actively enforced via curriculum mandates).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing generational rupture in script change) was live and real when the managed transition framework was established (~2010s). By 2025 (current point), the problem's status is contested: the state authority and international educators attest it is live (archive institutions still need dual-script custodians); rapid modernization advocates attest it is dead (digital archive tools and academic specialists have made the continuity problem obsolete); Ottoman continuity advocates attest the problem was never the transition's real target (the real problem was the 1928 reform itself). The sunset clause (2043) encodes an assumption that by that point the founding problem will be resolved (all elders deceased, archives converted to Latin-accessible formats, intergenerational bridge achieved). If 2043 arrives and the founding problem is *still* live (intergenerational transmission still incomplete, some institutions still functionally Arabic-dependent), the constraint will have become a piton: the state will need to either extend the sunset clause (explicitly admitting the founding problem is persistent and unresolved) or consolidate on Latin and accept the institutional disruption. The current trajectory (base_extractiveness declining, theater declining, suppression declining) suggests the policy is genuinely reducing coercive force, which is consistent with a well-managed scaffold. However, if the measurements show theater *rising* or suppression *stabilizing* (not declining) as the sunset approaches, that would indicate the policy is becoming performative and the sunset is a fiction—mandatrophy threshold crossed, constraint reclassified as piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence_at_sunset,
    'Will the founding problem (intergenerational knowledge rupture in script change) be sufficiently resolved by 2043 (the sunset date) to justify consolidating on Latin-only, or will the problem remain live?',
    'Observational milestone at t=2043: audit of archive institution staffing (are trained Arabic-script custodians succeeded by Latin-trained staff?), measurement of elderly intergenerational teaching rates (do young students of heritage institutions achieve reading competency in both scripts?), assessment of digitization completeness (is Ottoman heritage accessible in Latin-readable formats with sufficient scholarly apparatus?). If the founding problem is live, the policy fails its own justification.',
    'If the problem persists past sunset and the policy consolidates on Latin anyway, the constraint becomes a piton (extractive machinery that persists despite failure to solve its founding problem). If the problem is resolved and consolidation proceeds, the scaffold is validated. If the problem is contested at 2043 (some institutions claim resolution, others claim they were cut off prematurely), the constraint transitions to contested_closure and may be reclassified as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_at_sunset, empirical, 'Whether the managed transition achieves its stated purpose (intergenerational knowledge transfer) or becomes an extended performance of compassion while homogenization proceeds regardless.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the ottoman_continuity_reading and secular_nationalist_reading foreclose each other within a single state framework, or can they coexist as different factions'' positions on the same kernel?',
    'Political-structural analysis: can a Turkish state hold both readings simultaneously (e.g., Arabic script remains legitimate in religious/cultural domains while Latin remains primary in secular/administrative domains)? If yes, the readings coexist. If no—if one must suppress the other within the state apparatus—they foreclose each other.',
    'If the readings foreclose each other, this gradual_transition_reading is a temporary compromise between incompatible commitments; the sunset clause represents inevitable victory for one reading over the other. If the readings coexist structurally, the ''managed transition'' is a permanent settlement, and the sunset clause is theatrical—designed to satisfy both sides during a political window but not truly intended to consolidate Latin-primary forever.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the three readings represent genuinely distinct commitments or factions fighting for control of a single framework.').

omega_variable(
    suppression_mechanism_identity_locked_or_structural,
    'Is the suppression holding the dual-script policy in place structural (legal mandates, curriculum requirements, budget allocation) or internalized/identity-locked (educators and students have internalized the Latin-primary goal and see dual-script competency as a transitional chore, not as a genuine reclamation)?',
    'Post-sunset observational test: if the constraint is removed (Latin becomes sole-primary by policy), does the suppression persist (e.g., do students and teachers continue to seek Arabic literacy despite no requirement)? If suppression persists post-removal, it is partially internalized; if it collapses immediately, suppression was purely structural.',
    'If suppression is internalized, the extracted dual-literacy burden is carried into the post-sunset era (students bear cognitive load and identity confusion about the ''temporary'' nature of Arabic study even after it ends). If suppression is purely structural, the burden lifts at sunset. Identity-locked suppression is more damaging to intergenerational continuity (it makes young people view their heritage script as a residue, not a living practice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_locked_or_structural, empirical, 'Whether the constraint''s coercive force is external or internalized in the target populations'' beliefs about legitimate language and identity.').

omega_variable(
    reading_sibling_differential_authority_grounding,
    'What type of authority grounds each reading''s legitimacy claim? ottoman_continuity_reading: theological/lineage (religious authority, Ottoman inheritance). secular_nationalist_reading: expertise/extraction (technocratic modernization claims, state power to reshape identity). gradual_transition_reading: instrumental/conventional (managing a practical problem, parliamentary decision). If each reading rests on different authority types, can they coexist in one framework without one authority type suppressing the others?',
    'Structural analysis of state institutions: which authority types does the Turkish state apparatus recognize as legitimate? If the state recognizes expertise and convention but not theology or lineage authority equally, the secular_nationalist and gradual_transition readings are favored over ottoman_continuity_reading by the structural arrangement itself.',
    'The gradual_transition_reading claims to be neutral, temporary, and instrumental—but if the state''s underlying authority structure privileges secular/technocratic legitimacy over religious/lineage legitimacy, the ''transition'' is not truly neutral; it is a temporary accommodation of religious authority that the state''s own structure is designed to marginalize. The sunset clause, then, is not just a practical timeline—it is an authorization of the deeper structural logic that eventually deprioritizes religious/lineage authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_differential_authority_grounding, conceptual, 'Whether the gradual_transition_reading''s neutrality is genuine or masks a bias toward secular-technocratic authority grounding that will inevitably favor the secular_nationalist_reading when the sunset is enforced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 1928, 2043).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t1928, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1928, 0.92).
narrative_ontology:measurement_basis(turk_tr_t1928, projected).
narrative_ontology:measurement(turk_tr_t1980, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement_basis(turk_tr_t1980, observed).
narrative_ontology:measurement(turk_tr_t2010, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(turk_tr_t2010, observed).
narrative_ontology:measurement(turk_tr_t2025, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(turk_tr_t2025, observed).
narrative_ontology:measurement(turk_tr_t2035, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 2035, 0.28).
narrative_ontology:measurement_basis(turk_tr_t2035, projected).
narrative_ontology:measurement(turk_tr_t2043, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 2043, 0.15).
narrative_ontology:measurement_basis(turk_tr_t2043, projected).

% Extraction over time
narrative_ontology:measurement(turk_be_t1928, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1928, 0.85).
narrative_ontology:measurement_basis(turk_be_t1928, projected).
narrative_ontology:measurement(turk_be_t1980, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement_basis(turk_be_t1980, observed).
narrative_ontology:measurement(turk_be_t2010, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement_basis(turk_be_t2010, observed).
narrative_ontology:measurement(turk_be_t2025, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(turk_be_t2025, observed).
narrative_ontology:measurement(turk_be_t2035, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 2035, 0.52).
narrative_ontology:measurement_basis(turk_be_t2035, projected).
narrative_ontology:measurement(turk_be_t2043, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 2043, 0.41).
narrative_ontology:measurement_basis(turk_be_t2043, projected).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t1928, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1928, 0.88).
narrative_ontology:measurement_basis(turk_su_t1928, projected).
narrative_ontology:measurement(turk_su_t1980, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement_basis(turk_su_t1980, observed).
narrative_ontology:measurement(turk_su_t2010, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement_basis(turk_su_t2010, observed).
narrative_ontology:measurement(turk_su_t2025, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(turk_su_t2025, observed).
narrative_ontology:measurement(turk_su_t2035, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 2035, 0.58).
narrative_ontology:measurement_basis(turk_su_t2035, projected).
narrative_ontology:measurement(turk_su_t2043, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 2043, 0.38).
narrative_ontology:measurement_basis(turk_su_t2043, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1928, tn=2043
narrative_ontology:measurement(turk_grid_01, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(class), 1928, 0.95).
narrative_ontology:measurement_basis(turk_grid_01, projected).
narrative_ontology:measurement(turk_grid_02, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(class), 2043, 0.68).
narrative_ontology:measurement_basis(turk_grid_02, projected).
narrative_ontology:measurement(turk_grid_03, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(individual), 1928, 0.92).
narrative_ontology:measurement_basis(turk_grid_03, projected).
narrative_ontology:measurement(turk_grid_04, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(individual), 2043, 0.78).
narrative_ontology:measurement_basis(turk_grid_04, projected).
narrative_ontology:measurement(turk_grid_05, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(organizational), 1928, 0.88).
narrative_ontology:measurement_basis(turk_grid_05, projected).
narrative_ontology:measurement(turk_grid_06, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(organizational), 2043, 0.72).
narrative_ontology:measurement_basis(turk_grid_06, projected).
narrative_ontology:measurement(turk_grid_07, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(structural), 1928, 0.98).
narrative_ontology:measurement_basis(turk_grid_07, projected).
narrative_ontology:measurement(turk_grid_08, turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse(structural), 2043, 0.82).
narrative_ontology:measurement_basis(turk_grid_08, projected).
narrative_ontology:measurement(turk_grid_09, turkish_graphemic_substrate__gradual_transition_reading, resistance(class), 1928, 0.75).
narrative_ontology:measurement_basis(turk_grid_09, observed).
narrative_ontology:measurement(turk_grid_10, turkish_graphemic_substrate__gradual_transition_reading, resistance(class), 2043, 0.48).
narrative_ontology:measurement_basis(turk_grid_10, projected).
narrative_ontology:measurement(turk_grid_11, turkish_graphemic_substrate__gradual_transition_reading, resistance(individual), 1928, 0.72).
narrative_ontology:measurement_basis(turk_grid_11, observed).
narrative_ontology:measurement(turk_grid_12, turkish_graphemic_substrate__gradual_transition_reading, resistance(individual), 2043, 0.58).
narrative_ontology:measurement_basis(turk_grid_12, projected).
narrative_ontology:measurement(turk_grid_13, turkish_graphemic_substrate__gradual_transition_reading, resistance(organizational), 1928, 0.68).
narrative_ontology:measurement_basis(turk_grid_13, observed).
narrative_ontology:measurement(turk_grid_14, turkish_graphemic_substrate__gradual_transition_reading, resistance(organizational), 2043, 0.52).
narrative_ontology:measurement_basis(turk_grid_14, projected).
narrative_ontology:measurement(turk_grid_15, turkish_graphemic_substrate__gradual_transition_reading, resistance(structural), 1928, 0.78).
narrative_ontology:measurement_basis(turk_grid_15, observed).
narrative_ontology:measurement(turk_grid_16, turkish_graphemic_substrate__gradual_transition_reading, resistance(structural), 2043, 0.42).
narrative_ontology:measurement_basis(turk_grid_16, projected).
narrative_ontology:measurement(turk_grid_17, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(class), 1928, 0.92).
narrative_ontology:measurement_basis(turk_grid_17, projected).
narrative_ontology:measurement(turk_grid_18, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(class), 2043, 0.42).
narrative_ontology:measurement_basis(turk_grid_18, projected).
narrative_ontology:measurement(turk_grid_19, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(individual), 1928, 0.85).
narrative_ontology:measurement_basis(turk_grid_19, projected).
narrative_ontology:measurement(turk_grid_20, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(individual), 2043, 0.52).
narrative_ontology:measurement_basis(turk_grid_20, projected).
narrative_ontology:measurement(turk_grid_21, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(organizational), 1928, 0.88).
narrative_ontology:measurement_basis(turk_grid_21, projected).
narrative_ontology:measurement(turk_grid_22, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(organizational), 2043, 0.48).
narrative_ontology:measurement_basis(turk_grid_22, projected).
narrative_ontology:measurement(turk_grid_23, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(structural), 1928, 0.96).
narrative_ontology:measurement_basis(turk_grid_23, projected).
narrative_ontology:measurement(turk_grid_24, turkish_graphemic_substrate__gradual_transition_reading, stakes_inflation(structural), 2043, 0.38).
narrative_ontology:measurement_basis(turk_grid_24, projected).
narrative_ontology:measurement(turk_grid_25, turkish_graphemic_substrate__gradual_transition_reading, suppression(class), 1928, 0.95).
narrative_ontology:measurement_basis(turk_grid_25, projected).
narrative_ontology:measurement(turk_grid_26, turkish_graphemic_substrate__gradual_transition_reading, suppression(class), 2043, 0.38).
narrative_ontology:measurement_basis(turk_grid_26, projected).
narrative_ontology:measurement(turk_grid_27, turkish_graphemic_substrate__gradual_transition_reading, suppression(individual), 1928, 0.92).
narrative_ontology:measurement_basis(turk_grid_27, projected).
narrative_ontology:measurement(turk_grid_28, turkish_graphemic_substrate__gradual_transition_reading, suppression(individual), 2043, 0.48).
narrative_ontology:measurement_basis(turk_grid_28, projected).
narrative_ontology:measurement(turk_grid_29, turkish_graphemic_substrate__gradual_transition_reading, suppression(organizational), 1928, 0.88).
narrative_ontology:measurement_basis(turk_grid_29, projected).
narrative_ontology:measurement(turk_grid_30, turkish_graphemic_substrate__gradual_transition_reading, suppression(organizational), 2043, 0.42).
narrative_ontology:measurement_basis(turk_grid_30, projected).
narrative_ontology:measurement(turk_grid_31, turkish_graphemic_substrate__gradual_transition_reading, suppression(structural), 1928, 0.98).
narrative_ontology:measurement_basis(turk_grid_31, projected).
narrative_ontology:measurement(turk_grid_32, turkish_graphemic_substrate__gradual_transition_reading, suppression(structural), 2043, 0.35).
narrative_ontology:measurement_basis(turk_grid_32, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__gradual_transition_reading, 0.12).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% The turkish_graphemic_substrate kernel is contested across three readings, each instantiating a different constraint with different structural properties. gradual_transition_reading (this story) assumes both scripts remain legitimate during a managed 5-15 year transition; ottoman_continuity_reading treats Arabic as permanently legitimate; secular_nationalist_reading treats Latin as the sole legitimate substrate. The three readings have the same kernel (the legitimacy of Turkey's graphemic substrate) but different ε values (gradual_transition: ε=0.58; ottoman_continuity: lower extraction if coercion removed; secular_nationalist: higher extraction if dual-script required). Each reading is a separate constraint story; they are linked here as a constraint family. The gradual_transition_reading influences both siblings by establishing a temporal window in which the contest is deferred; whichever sibling 'wins' at the sunset (2043) will determine the post-transition landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, powerless, 0.88).
constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, moderate, 0.75).
constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
