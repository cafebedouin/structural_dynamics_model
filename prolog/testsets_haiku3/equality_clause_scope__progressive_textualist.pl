% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope (Progressive Textualist Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The progressive textualist reading of the equality clause claims that a
 *   written equality principle is present in the constitutional text, but its
 *   scope (which groups, which contexts, which hierarchies it constrains)
 *   cannot be expanded through judicial reinterpretation alone. Instead,
 *   scope expansion requires democratic amendment — supermajority legislative
 *   or popular consent. This reading steers between two extremes: it rejects
 *   the idea that courts can read contemporary values into an old text
 *   (opposing expansive reinterpretation), but it also rejects the idea that
 *   the text is frozen at its original scope forever (opposing pure
 *   originalism). The extraction cost lies in the gap between when a group is
 *   excluded from equal protection and when a supermajority amendment happens
 *   — if it ever does. The constraint structurally privileges the status quo
 *   and democratic gatekeepers over excluded groups seeking immediate
 *   inclusion.
 *
 * KEY AGENTS:
 *   - Democratic supermajority gatekeepers: legislatures, state conventions, voters at amendment referenda; set the approval standard; benefit from delay
 *   - Excluded groups seeking immediate inclusion: slaves, women, religious minorities, colonial subjects, sexual minorities; bear the cost of waiting; cannot exit the jurisdiction
 *   - Courts and judicial interpreters: maintain interpretive authority over existing scope; barred from unilaterally expanding scope; constrained by the reading's rule
 *   - Institutional continuity defenders: existing power holders, established state institutions; benefit from amendment gate that prevents reinterpretation
 *   - Originalist legal traditionalists: benefit from text-based legitimacy and democratic amendment path; positioned as moderate between expansive courts and frozen originalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.58).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.62).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope (Progressive Textualist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'f9610f8d-8380-43c8-8e79-5d52c380e90e').
narrative_ontology:cs_kernel_codification('f9610f8d-8380-43c8-8e79-5d52c380e90e', fixed_text).
narrative_ontology:cs_authority_grounding('f9610f8d-8380-43c8-8e79-5d52c380e90e', lineage).
narrative_ontology:cs_interpretation_layer_present('f9610f8d-8380-43c8-8e79-5d52c380e90e').
narrative_ontology:cs_reading_relation('f9610f8d-8380-43c8-8e79-5d52c380e90e', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('f9610f8d-8380-43c8-8e79-5d52c380e90e', equality_clause_scope__expansive_universalist, influences).
narrative_ontology:cs_axiom('f9610f8d-8380-43c8-8e79-5d52c380e90e', foundational, text_constrains_scope_expansion).
narrative_ontology:cs_axiom_status(text_constrains_scope_expansion, holdable).
narrative_ontology:cs_axiom_grounding('f9610f8d-8380-43c8-8e79-5d52c380e90e', text_constrains_scope_expansion, conventional).
narrative_ontology:cs_axiom('f9610f8d-8380-43c8-8e79-5d52c380e90e', foundational, supermajority_democratic_legitimacy_threshold).
narrative_ontology:cs_axiom_status(supermajority_democratic_legitimacy_threshold, holdable).
narrative_ontology:cs_axiom_grounding('f9610f8d-8380-43c8-8e79-5d52c380e90e', supermajority_democratic_legitimacy_threshold, conventional).
narrative_ontology:cs_axiom('f9610f8d-8380-43c8-8e79-5d52c380e90e', secondary, courts_interpret_not_redefine_scope).
narrative_ontology:cs_axiom_status(courts_interpret_not_redefine_scope, holdable).
narrative_ontology:cs_axiom_grounding('f9610f8d-8380-43c8-8e79-5d52c380e90e', courts_interpret_not_redefine_scope, deontological).
narrative_ontology:cs_reference_frame('f9610f8d-8380-43c8-8e79-5d52c380e90e', text_fixed_scope_amendment_path).
narrative_ontology:cs_drift_state('f9610f8d-8380-43c8-8e79-5d52c380e90e', contemporary_pluralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9610f8d-8380-43c8-8e79-5d52c380e90e', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_supermajority_gatekeepers).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, institutional_continuity_defenders).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, excluded_groups_seeking_immediate_inclusion).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, subordinate_publics_subject_to_existing_definitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, originalist_legal_traditionalists).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, courts_and_judicial_interpreters).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, constitutional_amendment_supremacy_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, supermajority_as_democratic_legitimacy_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the amendment process (legislatures, state conventions, ratification assemblies depending on constitutional structure). Set the barrier to expanding equality's scope: supermajority agreement required before the text's application widens. Benefit from stable, predictable ratchet that requires their approval for inclusion. Enforce through gatekeeping, not through interpretation authority alone.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_supermajority_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the cost of exclusion from equal protection until supermajority consent materializes. Cannot exit the jurisdiction; cannot redefine the constraint through legal argument (the reading bars that exit). Must wait for amendment, mobilize supermajority support, or accept continued subordination. The waiting period is their extraction cost.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, excluded_groups_seeking_immediate_inclusion, payer,
    powerless, biographical, trapped, national).

% Benefit from a reading that protects existing institutional arrangements (state legislation, constitutional structure, property distributions, social hierarchies) from reinterpretation. The amendment gate prevents courts from unilaterally expanding equality's scope beyond what the historical text said, preserving institutional stability and existing power distributions. Their position is secured by democratic supermajority requirement, not by the reading's intrinsic merit.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, institutional_continuity_defenders, beneficiary,
    institutional, generational, analytical, national).

% Constrained by the reading's rule: interpretation authority is narrower than in expansive readings; courts cannot extend equality's scope beyond the text's original application without waiting for amendment. They retain authority to interpret what the existing scope covers, but cannot expand which groups fall within it. The constraint limits their power while preserving their role as arbiters of existing textual meaning.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, courts_and_judicial_interpreters, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, courts_and_judicial_interpreters, observer).

% Pay through continued subjection to classifications that the existing equal-protection text does not reach or does not constrain (slavery, coverture, racial codes, gender hierarchies). They bear the cost of waiting for supermajority consensus to expand the text's application. Their ability to challenge their own exclusion through courts is foreclosed: they must await democratic amendment, which may never arrive or may arrive after generations.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, subordinate_publics_subject_to_existing_definitions, payer,
    moderate, biographical, constrained, national).

% Benefit from a reading grounded in text and amendment process, which aligns with originalist jurisprudence. The progressive textualist reading occupies a moderate position: original meaning matters (opposing expansive reinterpretation), but democratic amendment is the path to change (opposing indefinite freezing of meaning). They find this reading more legitimate than expansive courts but more progressive than pure originalism.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, originalist_legal_traditionalists, beneficiary,
    organized, generational, mobile, national).

% Would have a voice if the constraint permitted them to use it. They mobilize supermajorities to expand equality's scope, but the supermajority requirement itself is their barrier: they must convince large coalitions spanning regions, factions, and interests. Without amendment, their policy goals are blocked. They are systematically excluded from judicial pathways to inclusion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, amendment_coalition_builders, excluded,
    moderate, biographical, constrained, national).

% Observe how this reading navigates the tension between text stability and democratic responsiveness. Other constitutional systems make different choices (constitutional courts with broader interpretation authority, supermajority parliaments with simpler amendment). Their different solutions illuminate what the progressive textualist trade-off gives up and protects.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, comparative_constitutional_democracies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional meaning around a fixed text and a known process for legitimate change. Courts maintain interpretive authority over the scope the text already covers, while democratic institutions maintain exclusive authority over expanding which groups/contexts fall within that scope. This prevents both judicial overreach and indefinite constitutional stasis: interpretation is stable, amendment is possible but hard.
% TRANSFER_FUNCTION: Transfers the cost of exclusion from excluded groups to the amendment coalition-builders (who must mobilize supermajorities). Transfers interpretive authority from courts to the democratic amendment process for scope-expansion questions. Transfers legitimacy from judicial wisdom to supermajority consent as the ground for constitutional change.
% ABSENT_VOICES: Excluded groups (slaves, women, religious minorities, colonial subjects, sexual minorities) have no voice in this framing because the reading treats them as outside the text's original scope and therefore outside judicial authority to include. Amendment requires their mobilization, but the reading itself silences them as legal claimants. Their objection would be that exclusion from both text and court doctrine is systematic disenfranchisement dressed as democratic legitimacy.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and courts instead wielded full reinterpretive authority (expansive reading) or if the text itself were held as unamendable (restrictive originalism), the entire landscape of constitutional rights claims would reorganize. Excluded groups could petition courts directly rather than waiting for amendments; or amendments would become impossible, freezing rights to original scope. Institutional power distributions would shift dramatically.
% FOUNDING_PROBLEM: How to preserve a written constitution against both arbitrary judicial reinterpretation and political despotism? The founding anxiety was that courts would legislate from the bench or that majorities would ignore constitutional limits. The reading proposes: courts interpret the text as written, but the people (through supermajority amendment) can change what the text says.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars (Randy Barnett, Keith Whittington) and constitutional historians (Jack Rakove) attest the framers feared both judicial usurpation and majoritarian tyranny. Progressive scholars (Cass Sunstein, Jack Balkin) and voting-rights advocates attest the supermajority amendment process itself excludes subordinate groups and has become effectively immutable on many issues. Courts in the United States and comparative constitutional democracies offer varying readings: some embrace broad judicial authority (South African Constitutional Court), others emphasize amendment formality (German Basic Law), others split the difference (Canadian Charter with notwithstanding clause). No single outside authority declares the founding problem definitively solved or unsolved; the corroboration is distributed across legal traditions in tension.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.58 at interval end) measures the cost imposed on excluded groups by requiring supermajority amendment rather than permitting judicial inclusion. The cost rises from 0.42 to 0.58 over the interval as legal mobilization by excluded groups increases and the amendment process becomes the focal point — they invest effort in amendment campaigns that fail (supermajority is never reached) or succeed slowly (decades between mobilization and ratification). Suppression (0.62) measures the active work of gatekeepers to prevent courts from expanding scope unilaterally — they argue for narrow interpretation, originalist methodology, and amendment-only legitimacy. Theater (0.41) reflects the fact that some amendment-process activity is ceremonial (ratification votes that are foregone conclusions once legislatures align) while some is genuinely contested. The measurement series share one time grid: all three metrics are authored at t=0,10,20,30,40,50. The trajectory shows extractiveness plateauing after t=30 (the amendment process reaches its sustainable operating rhythm and successful amendments grow rarer as the supermajority threshold remains high), theater rising and plateauing (the performance of amendment debate becomes routinized), and suppression stabilizing at high levels (gatekeepers maintain consistent effort to block judicial expansion). The dip in suppression_requirement at t=50 (projected) reflects uncertainty about whether gatekeeping effort can sustain indefinitely or will erode as excluded groups find alternative venues (international human rights bodies, competing legal traditions).
 *
 * PERSPECTIVAL GAP:
 *   The supermajority gatekeeper seat and the excluded-group seat compute dramatically different types from identical structural data. Gatekeepers see legitimate democratic legitimacy thresholds protecting constitutional stability. Excluded groups see systematic disenfranchisement dressed as democracy. This gap is exactly what the per-seat computation reveals: same constraint, different classification by structural position. The engine's job is to compute that divergence, not to eliminate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the supermajority gatekeepers (institutional power, high exit via arbitrage — they can shift their coalition positions — d near 0.2) and institutional continuity defenders (institutional power, analytical exit — they observe but don't directly exit — d near 0.25). Victims are excluded groups (powerless, trapped exit — they cannot leave the jurisdiction — d near 0.95) and subordinate publics subject to existing definitions (moderate power, constrained exit — they can mobilize but within the amendment system's rules — d near 0.75). Courts sit near symmetric (institutional power, constrained exit in this specific constraint, direct benefit from preserved role but cost from narrowed authority — d near 0.5). The directionality override is not necessary here: the derivation from beneficiary/victim + exit captures the structural asymmetry accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protect constitutionalism against both judicial usurpation and majoritarian tyranny) is in contested status: originalists and textualists attest it is live and well-served by the amendment-gate mechanism; voting-rights advocates and subordinate groups attest it is dead (courts are no longer aggressive expansionists; amendment is effectively frozen). The disappearance verdict is world_rearranges: if the amendment requirement were removed and courts could directly expand scope, institutional configurations would change immediately. If the amendment were made impossible (pure originalism), rights would freeze at 1787 or 1868 scope. The contested founding-problem status + world-rearranges verdict suggests the constraint's mandate has partially outlived its stated function but persists because it serves distributional interests (gatekeepers' benefits from delay). This is incipient mandatrophy: the founding problem is no longer obviously live, but the constraint persists in its original form because changing it would require the supermajority that benefits from it to vote against themselves. The theater measurement (0.41 and rising) tracks the increasing ceremonial quality of some amendment processes while genuine contestation narrows to a few issues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_immutability_vs_supermajority_capacity,
    'Can the amendment process actually deliver supermajority expansion of equality''s scope, or has it become effectively frozen by structural barriers (federalism, polarization, veto players)?',
    'Historical observation: count successful amendments that expanded equality''s scope (13th, 14th, 15th, 19th, 24th, 26th) and measure intervals between proposal and ratification. Examine failed proposals (ERA, proportional representation amendments) and identify blocking coalitions. Assess whether future amendments remain possible or have become politically impossible.',
    'If amendment is effectively frozen, the progressive textualist reading collapses into restrictive originalism in practice, and the supermajority gate becomes pure exclusion rather than democratic legitimacy. If amendment remains viable, the reading preserves a genuine path to inclusion, and the cost is waiting time, not permanent exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_immutability_vs_supermajority_capacity, empirical, 'Whether the amendment process remains a live mechanism for expanding equality''s scope or has become effectively immutized by structural factors.').

omega_variable(
    judicial_reinterpretation_vs_legitimate_change,
    'Is there a principled distinction between judicial reinterpretation of the equality principle''s scope and democratic amendment, or is the boundary itself constructed by the reading''s framing?',
    'Comparative constitutional study: examine how other democracies allocate scope-expansion authority (constitutional courts, parliaments, popular referenda). Analyze whether the court/amendment boundary is a feature of the text or a feature of the reading''s interpretive choice. Test whether courts using ''living constitution'' methodology reach substantially different equality outcomes than amendment-based approaches.',
    'If the boundary is textual (courts really cannot reinterpret), the reading is objectively justified. If the boundary is chosen by the reading to preserve certain institutional positions, the reading is revealed as protecting gatekeepers under the cover of textual fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_reinterpretation_vs_legitimate_change, conceptual, 'Whether the court/amendment boundary is inherent to the text or constructed by the reading.').

omega_variable(
    supermajority_as_democratic_legitimacy_proxy,
    'Does requiring supermajority consent for scope expansion actually ensure democratic legitimacy, or does it mask the exclusion of permanent minorities from equal protection?',
    'Democratic theory analysis: examine whether supermajority rules protect fundamental rights or entrench majority tyranny over minorities. Historical case studies of minorities that never achieved supermajority support but eventually received equal protection (religious minorities, sexual minorities). Assess whether the supermajority standard itself is democratically legitimate or merely institutionally entrenched.',
    'If supermajority genuinely ensures legitimate change, the reading preserves democratic agency. If supermajority entrenches permanent minorities'' exclusion, the reading is a snare disguised as coordination, and excluded groups bear extraction costs indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_as_democratic_legitimacy_proxy, preference, 'Whether supermajority consent is a legitimate democratic threshold or a structural device for entrenchment.').

omega_variable(
    original_scope_boundary_ambiguity,
    'Is the original scope of the equality principle well-defined and recoverable from the text, or is the boundary itself contestable (requiring interpretation to establish)?',
    'Originalist and textualist scholarship on the 14th Amendment''s original public meaning. Examine whether competent originalist judges disagree on whether the original scope included women, enslaved people, non-citizens, or same-sex couples. If disagreement exists among originalists, the boundary itself requires interpretation, undermining the reading''s claim that courts cannot authoritatively determine scope.',
    'If the original boundary is clear and textually fixed, courts have genuinely limited authority to expand. If the boundary itself is contestable, courts necessarily interpret the scope, and the distinction between ''interpretation within existing scope'' and ''expansion beyond scope'' collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_scope_boundary_ambiguity, empirical, 'Whether the original scope of the equality clause is textually determinate or requires contestable interpretation.').

omega_variable(
    kernel_reading_displacement_under_factual_change,
    'If historical facts change in ways the framers did not contemplate (new technologies, new forms of discrimination, new groups emerging), does the progressive textualist reading permit scope expansion through reinterpretation of those facts within the original principle, or does it require amendment even for factual scenarios the original text could not address?',
    'Examine how courts applying progressive textualist methodology handle novel discrimination (disability, algorithmic discrimination, climate impact on equality). Assess whether the reading permits courts to apply the original equality principle to new contexts without amendment, or whether it requires amendment even for contexts the original drafters could not imagine.',
    'If the reading permits reinterpretation to new factual contexts, its scope may expand substantially without amendment, partially undermining its own amendment-gate. If it requires amendment even for novel factual scenarios, excluded groups facing new forms of discrimination must wait for supermajority recognition of those forms — extraction extends to novel harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_displacement_under_factual_change, conceptual, 'Whether the reading''s scope-expansion gate applies to new factual contexts or only to new groups/definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__progressive_textualist, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(equa_tr_t10, observed).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__progressive_textualist, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(equa_tr_t20, observed).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__progressive_textualist, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(equa_tr_t30, observed).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__progressive_textualist, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__progressive_textualist, theater_ratio, 50, 0.43).
narrative_ontology:measurement_basis(equa_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__progressive_textualist, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(equa_be_t10, observed).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__progressive_textualist, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(equa_be_t20, observed).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__progressive_textualist, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(equa_be_t30, observed).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__progressive_textualist, base_extractiveness, 40, 0.59).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__progressive_textualist, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(equa_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__progressive_textualist, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(equa_su_t10, observed).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__progressive_textualist, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(equa_su_t20, observed).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__progressive_textualist, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(equa_su_t30, observed).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__progressive_textualist, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__progressive_textualist, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(equa_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__progressive_textualist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, amendment_process_supermajority_requirement).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, judicial_interpretation_authority_boundary).

% DUAL FORMULATION NOTE:
% The equality_clause_scope kernel comprises three constraint stories instantiating different readings: (1) progressive_textualist (this file) — text contains equality, scope expands via amendment, moderate legitimacy; (2) restrictive_originalist — equality frozen at original scope, no valid amendment path; (3) expansive_universalist — equality is universal principle, courts can extend it to all humans. Each reading has different ε, different beneficiary/victim structure, different classification. All three stories share the same referent (the constitutional commitment to some form of equality) but instantiate different constraints because the readings disagree on what 'equality's scope' means structurally. Stories are linked via network.affects_constraints to reflect kernel dependency: progressive_textualist reading influences (creates structural pressure on) both sibling readings by offering a moderate position that may absorb some expansive pressure and some originalist legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
