% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Dual-Source Sovereign Legitimacy (Constitutional Hybrid)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested kernel: sovereign
 *   legitimacy. The constitutional hybrid reading asserts that legitimate
 *   authority is dual-sourced — ceremonial/symbolic authority inherited
 *   through bloodline and political authority delegated through democratic
 *   consent — with constitutional law mediating the boundary between them.
 *   This is ONE of three structurally distinct claims about the source of
 *   sovereign authority. The monarchical reading grounds legitimacy in divine
 *   right and hereditary succession; the republican reading grounds it
 *   entirely in popular sovereignty. The hybrid reading uniquely asserts that
 *   BOTH sources are legitimate within a single constitutional framework,
 *   mediated by written law and judicial interpretation. This reading has
 *   shaped the legitimacy settlements of the United Kingdom, the Netherlands,
 *   Spain, Belgium, Sweden, Denmark, Norway, and other constitutional
 *   monarchies. The constraint's operation depends on continuous
 *   constitutional interpretation to clarify and defend the boundary between
 *   the two sources.
 *
 * KEY AGENTS:
 *   - Hereditary monarch: retains ceremonial status, symbolic authority, income from crown property; subordinated to constitutional law and parliamentary legislation.
 *   - Elected officials: hold delegated political authority to legislate and govern; must acknowledge the monarch's constitutional role.
 *   - General citizenry: benefit from legitimacy stability combining symbolic continuity with responsive change; constrained within the system.
 *   - Absolutist partisans: seek to restore full monarchical authority; constrained by the constitutional framework.
 *   - Republican partisans: seek to abolish the monarchy and establish pure popular sovereignty; constrained by constitutional entrenchment.
 *   - Constitutional courts: mediate disputes about the boundary; interpret what each source can legitimately do.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.38).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.42).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Dual-Source Sovereign Legitimacy (Constitutional Hybrid)").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '5022350d-0854-4abe-b34a-1606cca86f1c').
narrative_ontology:cs_kernel_codification('5022350d-0854-4abe-b34a-1606cca86f1c', formalized).
narrative_ontology:cs_authority_grounding('5022350d-0854-4abe-b34a-1606cca86f1c', lineage).
narrative_ontology:cs_interpretation_layer_present('5022350d-0854-4abe-b34a-1606cca86f1c').
narrative_ontology:cs_reading_relation('5022350d-0854-4abe-b34a-1606cca86f1c', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('5022350d-0854-4abe-b34a-1606cca86f1c', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('5022350d-0854-4abe-b34a-1606cca86f1c', foundational, dual_legitimacy_sources_simultaneously_operative).
narrative_ontology:cs_axiom_status(dual_legitimacy_sources_simultaneously_operative, holdable).
narrative_ontology:cs_axiom_grounding('5022350d-0854-4abe-b34a-1606cca86f1c', dual_legitimacy_sources_simultaneously_operative, deontological).
narrative_ontology:cs_axiom('5022350d-0854-4abe-b34a-1606cca86f1c', foundational, constitutional_law_mediates_source_boundary).
narrative_ontology:cs_axiom_status(constitutional_law_mediates_source_boundary, holdable).
narrative_ontology:cs_axiom_grounding('5022350d-0854-4abe-b34a-1606cca86f1c', constitutional_law_mediates_source_boundary, conventional).
narrative_ontology:cs_axiom('5022350d-0854-4abe-b34a-1606cca86f1c', secondary, neither_source_monopolizes_state_authority).
narrative_ontology:cs_axiom_status(neither_source_monopolizes_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('5022350d-0854-4abe-b34a-1606cca86f1c', neither_source_monopolizes_state_authority, instrumental).
narrative_ontology:cs_reference_frame('5022350d-0854-4abe-b34a-1606cca86f1c', dual_source_constitutional_legitimacy).
narrative_ontology:cs_drift_state('5022350d-0854-4abe-b34a-1606cca86f1c', contemporary_post_industrial, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5022350d-0854-4abe-b34a-1606cca86f1c', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_partisans).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_partisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, general_citizenry).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial authority, symbolic primacy, and income derived from crown property and state pageantry. Political power is delegated to elected officials; the monarch's authority is bounded by constitutional law and parliamentary procedure. Benefits from the constraint by preserving status and institutional continuity; pays through loss of direct policy control and subordination to constitutional interpretation. Exit would mean renouncing the crown, which dissolves the identity.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, payer).

% Hold delegated political authority to make law and policy. Benefit from the constraint because popular sovereignty claim is validated by explicit delegation narrative rather than undermined by competing hereditary claim. They set the rules of governance within constitutional bounds. Pay through obligation to acknowledge the monarch's role and through the friction of constitutional interpretation that can constrain their actions.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter).

% Gain legitimacy stability from a system that combines symbolic continuity (the monarch embodies national identity across generations) with responsive political change (elected officials answer to contemporary electorate). They cannot directly exit the system but can migrate; they benefit from the institutional separation reducing the concentration of power in either hereditary or populist form alone.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, general_citizenry, beneficiary,
    organized, biographical, constrained, national).

% Argue that legitimate authority flows from inherited sovereignty and divine sanction; the constitutional hybrid subordinates that claim to popular delegation. They are constrained from pursuing full monarchical authority by the constitutional framework. Can advocate and litigate but cannot overturn the basic structure without revolutionary action.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_partisans, payer,
    moderate, biographical, constrained, national).

% Argue that legitimate authority must flow entirely from popular sovereignty; the constitutional hybrid preserves hereditary privilege alongside elected power. They are constrained from abolishing the monarchy by constitutional entrenchment and the institutional power of the crown. Can advocate and campaign but cannot dissolve the structure through electoral means alone.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_partisans, payer,
    moderate, biographical, constrained, national).

% Interpret the boundary between ceremonial and political authority through constitutional jurisprudence. They mediate disputes between the monarch and elected officials, and they clarify what the hybrid arrangement permits and forbids. Their rulings shape how the constraint operates and can shift the balance between the sources of authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, agenda_setter,
    institutional, generational, mobile, national).

% The doctrine that legitimate authority derives from inherited sovereignty and divine sanction is vindicated by the institutional preservation of the monarchy within the constitutional order. The tradition survives through the crown's retained symbolic role, even though political authority is delegated.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, historical_royalist_tradition, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(sovereign_legitimacy__constitutional_hybrid_reading, historical_royalist_tradition).

% The doctrine that legitimate authority derives from the people's consent and delegation is vindicated by the constitutional framework's assignment of policy-making power to elected representatives. Popular sovereignty is operationalized through the electoral process and parliamentary legislation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, enlightenment_popular_sovereignty_doctrine, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(sovereign_legitimacy__constitutional_hybrid_reading, enlightenment_popular_sovereignty_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the legitimacy conflict between hereditary and popular sources of authority by institutionally separating them: the monarch provides symbolic continuity, national identity, and historical legitimacy; elected officials provide responsive policy-making grounded in contemporary consent. The arrangement allows both claims to coexist without either monopolizing state authority.
% TRANSFER_FUNCTION: Moves political power from the monarchy to elected representatives while preserving the crown's ceremonial status, income from crown property, and role as head of state. Transfers symbolic authority to a living person (the monarch) and policy authority to a rotating group of elected officials. Both absolutists and republicans transfer their unconditional authority claims to a constitutional settlement where each source is bounded.
% ABSENT_VOICES: Revolutionary movements seeking to dissolve the monarchy entirely, and reactionary forces seeking to restore absolute monarchical rule, are both structurally excluded from the constitutional consensus. They would argue for pure form (all power from bloodline, or all power from the people) but are constrained to operate within the hybrid framework or outside the constitutional order entirely.
% DISAPPEARANCE_RATIONALE: If the dual-source arrangement disappeared, successor regimes would emerge within weeks: either a restored absolute monarchy (if the constraint was abolished by monarchist coup), a complete republic (if abolished by republican revolution), or a new constitutional settlement. The disappearance would trigger institutional reorganization and potentially civil conflict as the two excluded pure-form factions competed for the now-empty center.
% FOUNDING_PROBLEM: The legitimacy crisis of the 17th–18th centuries: monarchical absolutism claimed divine right and hereditary succession; rising merchant and professional classes claimed legitimacy grounded in consent and contract; no single source could command unified allegiance without suppressing the other. The constitutional settlement was designed to acknowledge both sources, mediated by written constitutional law, as a way to stabilize legitimacy without requiring either faction to renounce its foundational claim.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and political theorists external to either partisan faction attest that the founding problem (legitimacy via heredity vs. consent) was the driving force behind the settlement. Monarchists and republicans each contest whether the problem is truly resolved (monarchists argue the republican gains have eroded the crown's real authority; republicans argue ceremonial preservation is a loophole for restoration). Comparative constitutional analysis of post-revolutionary France, Germany, and Spain shows the same tension re-emerging when the hybrid is dismantled.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end), not high, because the hybrid arrangement reduces the extraction potential of both pure forms: the monarchy cannot extract through absolute rule, and elected officials cannot extract through unconstrained popular majoritarianism. However, the constraint does extract from both absolutist and republican partisans, who are forced to operate within the hybrid framework rather than pursue their preferred legitimacy principle. The theater ratio rises from 0.35 to 0.51 over the interval, indicating that as the foundational legitimacy problem fades into historical distance, an increasing share of monarchical ceremonial activity becomes performative maintenance of the boundary rather than active coordination of legitimacy. By interval end, roughly half of what the monarchy does is theater (state visits, honors, pageantry) that vindicates the hybrid arrangement without performing the original coordination function (resolving the acute legitimacy crisis). Suppression remains moderate (0.42) because the constraint's persistence depends on constitutional courts and elected legislatures actively defending the boundary against both pure-form factions, but neither faction has the organized power to mount sustained armed resistance — their suppression is mainly cognitive (acceptance of the framework) and electoral (constrained to working within it). The measurements are authored on a single shared time grid at intervals spanning 50 years (roughly two generations), showing a pattern of initial rise (as extractiveness and theater ratio settle toward their steady state) followed by stabilization (the hybrid becomes normalized and institutionalized).
 *
 * PERSPECTIVAL GAP:
 *   The hereditary monarch and elected officials should compute from different seats with different directionalities. The monarch is a structural beneficiary (retains status and income; d near 0.2–0.3); elected officials are also beneficiaries but with a different gain profile (they gain policy power without hereditary legitimacy challenges; d near 0.25–0.35). Absolutists and republicans are both victims (constrained from pursuing their preferred principle; d near 0.65–0.75). Constitutional courts are agenda-setters (they mediate the boundary and can shift its location through interpretation; d near 0.5, symmetric). The engine should compute partisan seats as more highly extractive targets than beneficiary seats, even though all participate in the same institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch benefits by retaining institutional position, income, and symbolic authority despite loss of political power — the alternative (abolition) is far worse from their perspective. However, they also pay through subordination to constitutional law and parliamentary oversight. Exit is identity-locked (renouncing the crown means ceasing to be the monarch). Elected officials benefit by gaining policy power and popular sovereignty legitimacy without having to abolish or overthrow the crown — the alternative (attempting to eliminate the monarchy) is politically costly and legally blocked. They pay through the obligation to acknowledge the crown's constitutional role. Republicans and absolutists pay by being forced to operate within the hybrid framework; they cannot pursue their preferred legitimacy principle without revolutionary action. Constitutional courts gain agenda-setting power to interpret the boundary — they are positioned symmetrically between the two beneficiary factions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimacy crisis between hereditary and popular sources) was live and urgent in the 17th–18th centuries. By the interval's present (roughly 200+ years post-establishment), the problem has become contested: monarchists argue the settlement has been eroded by republican gains and the crown is now merely decorative; republicans argue the settlement preserves hereditary privilege as an anachronism in a democratic age. The theater ratio rising to 0.51 suggests the constraint is approaching mandatrophy — an increasing share of what the monarchy does is performance maintaining the boundary rather than active coordination of a live legitimacy problem. However, the constraint does not yet qualify as a full piton because: (1) the boundary disputes still matter constitutionally (courts regularly adjudicate them), (2) no single actor has taken over the maintenance function from the founding problem's resolution (the arrangement is still defended by committed beneficiaries on both sides), and (3) the measurement series shows stabilization rather than continued decay (theater ratio plateaus after rising). If the theater ratio continued to rise above 0.65 and elected officials began treating monarchical constraints as purely ceremonial rather than constitutional, the constraint would cross into piton territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_interpretability_drift,
    'As the foundational legitimacy crisis recedes into history, can constitutional courts continue to maintain a coherent boundary between ceremonial and political authority, or will the distinction erode into incoherence?',
    'Observe constitutional jurisprudence over the next generation: if courts consistently apply the boundary to novel questions (e.g., digital-age state representation, emergency powers), the distinction is stable; if courts begin treating the boundary as merely symbolic or unenforceable, the distinction has eroded.',
    'If the boundary erodes to incoherence, the constraint shifts from tangled_rope (coordination + extraction) toward piton (pure performance maintaining an atrophied function). If the boundary remains enforceable, the constraint remains tangled_rope with ongoing coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_interpretability_drift, empirical, 'Whether the constitutional boundary between ceremonial and political authority can remain interpretively coherent as the founding legitimacy problem becomes historical.').

omega_variable(
    legitimacy_monism_resurgence,
    'Will absolutist or republican partisans successfully overturn the hybrid arrangement and restore pure-form legitimacy (either monarchical or republican)?',
    'Monitor constitutional reform efforts, revolutionary movements, and electoral outcomes in constitutional monarchies: if a major Western constitutional monarchy transitions to either absolute monarchy or pure republic within 50 years, the hybrid reading has been foreclosed by one of its sibling readings. Otherwise, the coexistence remains.',
    'If the hybrid is overturned, this constraint ceases to operate and one of the sibling readings (monarchical or republican) becomes the operative legitimacy claim. If the hybrid persists, it confirms that dual-source legitimacy is stable against pure-form pressures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_monism_resurgence, empirical, 'Whether the constitutional hybrid legitimacy settlement is stable against pressure from pure-form rivals.').

omega_variable(
    inherited_legitimacy_identity_lock,
    'Is the hereditary monarch truly identity-locked to the crown (exit would mean ceasing to be oneself), or is this an internalized constraint that could be overcome by a sufficiently motivated monarch seeking to abdicate and live as a private citizen?',
    'Case studies of monarchs who have abdicated: do they experience the abdication as a loss of self (identity dissolution) or as a liberation from an imposed role? What percentage of historical abdications were voluntary vs. coerced? Do abdicated monarchs report post-abdication psychological integration or ongoing identity fragmentation?',
    'If identity-lock is truly constitutive, exit options remain severely constrained and the monarch''s participation in the constraint is forced. If identity-lock is partially internalized and could be overcome, the constraint''s suppression is higher than the structural data suggests, and the monarch''s d value should be recomputed upward (closer to victimhood than beneficiary).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inherited_legitimacy_identity_lock, empirical, 'Whether the hereditary monarch''s identity-lock exit constraint is structural or partially internalized.').

omega_variable(
    dual_source_vs_sequential_source,
    'Is the constitutional hybrid reading truly claiming DUAL sources of legitimacy (both sources remain active simultaneously and can each validate authority independently), or is it actually a SEQUENTIAL reading where ceremonial authority is the legacy of past legitimacy and political authority is the only operative legitimacy in the present?',
    'Examine constitutional court rulings when the two sources conflict: if courts treat both sources as independently sufficient to validate authority (you can act as monarch OR as elected official; each source is legitimate on its own), the reading is truly dual. If courts treat ceremonial authority as historically necessary but now superseded by political authority (the crown is revered but not commanding), the reading is sequential.',
    'If sequential, the reading is closer to republican with a museum piece (the monarchy is preserved for continuity but has no operative legitimacy). If truly dual, the reading is architecturally distinct from both pure forms and represents a genuine third way. This affects whether the boundary is defensible or merely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_source_vs_sequential_source, conceptual, 'Whether the dual-source reading represents two simultaneously operative legitimacy sources or a sequential transition from one source to another.').

omega_variable(
    kernel_reading_assignment,
    'Is the constitutional hybrid reading a coherent instantiation of the sovereign_legitimacy kernel, or does it collapse under scrutiny into a disguised version of one of the pure-form readings?',
    'Test the reading''s internal consistency: (1) Does asserting dual sources commit you to believing the monarch has real (not merely ceremonial) authority? (2) Does asserting that both sources are legitimate require you to accept that elected officials can be overridden by the crown? (3) If yes to both, why do we observe that elected officials are not actually overrideable? If the answers reveal internal contradiction, the reading is incoherent and collapses into one of the pure forms. If coherent, the reading stands as a distinct constraint.',
    'If the reading is incoherent, it is not a viable constraint and should be reclassified as a false summit (claimed as hybrid but structurally republican with monarchical theater). If coherent, the reading is validated as architecturally distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_assignment, conceptual, 'Whether the dual-source reading is a coherent constraint or a disguised pure-form reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t8, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement_basis(sove_tr_t8, observed).
narrative_ontology:measurement(sove_tr_t16, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement_basis(sove_tr_t16, observed).
narrative_ontology:measurement(sove_tr_t24, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 24, 0.49).
narrative_ontology:measurement_basis(sove_tr_t24, observed).
narrative_ontology:measurement(sove_tr_t32, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement_basis(sove_tr_t32, observed).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement_basis(sove_tr_t40, observed).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement_basis(sove_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t8, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(sove_be_t8, observed).
narrative_ontology:measurement(sove_be_t16, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement_basis(sove_be_t16, observed).
narrative_ontology:measurement(sove_be_t24, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(sove_be_t24, observed).
narrative_ontology:measurement(sove_be_t32, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(sove_be_t32, observed).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(sove_be_t40, observed).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(sove_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t8, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(sove_su_t8, observed).
narrative_ontology:measurement(sove_su_t16, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement_basis(sove_su_t16, observed).
narrative_ontology:measurement(sove_su_t24, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(sove_su_t24, observed).
narrative_ontology:measurement(sove_su_t32, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement_basis(sove_su_t32, observed).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(sove_su_t40, observed).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(sove_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.18).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel decomposes into three architecturally distinct constraints: (1) monarchical_reading (ε~0.55, legitimacy from divine right and hereditary succession, high extraction from republicans), (2) republican_reading (ε~0.52, legitimacy from popular sovereignty, high extraction from absolutists), (3) constitutional_hybrid_reading (ε~0.38, legitimacy from dual sources mediated by constitutional law, lower extraction because compromise reduces both pure forms' asymmetries). The hybrid reading structurally depends on both pure-form readings remaining as live alternatives that it mediates between; if either pure form gains full control, the hybrid constraint ceases to operate. The network edge direction runs from hybrid to both pure forms because the hybrid's persistence as a constitutional settlement continuously pushes back against pure-form pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__constitutional_hybrid_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
