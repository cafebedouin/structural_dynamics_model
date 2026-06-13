% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Minoritarian Veto (Crown Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the CROWN READING of the
 *   remonstrance-authority kernel: remonstrance right as a mechanism whereby
 *   landed magistrate class and provincial nobility block or delay Crown
 *   fiscal innovation in the name of ancient constitutional duty, but which
 *   structurally protects particularist property privileges against universal
 *   reallocation of fiscal burden. The Crown reading frames remonstrance as
 *   illegitimate minoritarian veto—a constitutional doctrine that was
 *   originally restraint on arbitrary monarchy but has become a tool to
 *   prevent fiscal modernization and capacity building. The constraint's
 *   operation is substantially extractive: magistrate interests collect the
 *   benefit of blocked taxation (their relative fiscal advantage is
 *   preserved); the Crown and commercial/urban interests bear the cost
 *   (fiscal constraint and exclusion from the beneficiary set). Enforcement
 *   is active: magistrates and provincial assemblies must continuously assert
 *   legal and political challenge to each Crown fiscal edict; the Crown must
 *   continuously negotiate around or work through remonstrance to advance
 *   taxation. The measurement series track the intensification of extraction
 *   and theater over the long interval (1600–1789): as fiscal pressure on the
 *   Crown accumulated (wars, debt, institutional expansion), the
 *   extractiveness of remonstrance privilege rose—the magistracy increasingly
 *   rented out its constitutional blocking power; theater rose as the
 *   constitutional justification became more ornamental and the veto more
 *   instrumental.
 *
 * KEY AGENTS:
 *   - Landowning magistrate class: structural beneficiary and agenda-setter; uses remonstrance to block reallocation of fiscal burden from landed property to commerce.
 *   - Crown fiscal authority: structural victim; faces mounting fiscal pressure thwarted by remonstrance blocks; enters victim set when remonstrance defeats specific tax proposals.
 *   - Commercial and urban interests: structural victims, excluded from remonstrance process; absorb fiscal burdens magistracy blocks, or face constrained credit and policy access when Crown lacks revenue.
 *   - Provincial assemblies (estates, parlements): institutional vessels of remonstrance; their prestige depends on the legitimacy of the constitutional claim.
 *   - Hereditary nobility: beneficiary inseparable from magistrate class; protected by remonstrance against fiscal innovation without bearing organizational costs.
 *   - Popular commoners: excluded and trapped; experience fiscal consequences of blocked Crown taxation without voice in the outcome.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.71).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional/political").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '598d8203-4bf3-4166-801a-e7cb94a69a20').
narrative_ontology:cs_kernel_codification('598d8203-4bf3-4166-801a-e7cb94a69a20', fixed_text).
narrative_ontology:cs_authority_grounding('598d8203-4bf3-4166-801a-e7cb94a69a20', extraction).
narrative_ontology:cs_interpretation_layer_present('598d8203-4bf3-4166-801a-e7cb94a69a20').
narrative_ontology:cs_reading_relation('598d8203-4bf3-4166-801a-e7cb94a69a20', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('598d8203-4bf3-4166-801a-e7cb94a69a20', foundational, landed_property_privilege_is_illegitimate_veto).
narrative_ontology:cs_axiom_status(landed_property_privilege_is_illegitimate_veto, holdable).
narrative_ontology:cs_axiom_grounding('598d8203-4bf3-4166-801a-e7cb94a69a20', landed_property_privilege_is_illegitimate_veto, instrumental).
narrative_ontology:cs_axiom('598d8203-4bf3-4166-801a-e7cb94a69a20', secondary, fiscal_modernization_requires_central_authority).
narrative_ontology:cs_axiom_status(fiscal_modernization_requires_central_authority, holdable).
narrative_ontology:cs_axiom_grounding('598d8203-4bf3-4166-801a-e7cb94a69a20', fiscal_modernization_requires_central_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('598d8203-4bf3-4166-801a-e7cb94a69a20', sovereign_crown_fiscal_authority).
narrative_ontology:cs_drift_state('598d8203-4bf3-4166-801a-e7cb94a69a20', late_eighteenth_century, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('598d8203-4bf3-4166-801a-e7cb94a69a20', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, landowning_magistrate_class).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, commercial_urban_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, hereditary_nobility).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, provincial_assemblies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Landed provincial magistrates and hereditary local power-holders who exercise remonstrance rights to block or delay royal fiscal edicts that would alter local tax burdens, tolls, or labor obligations. They frame remonstrance as constitutional duty; in practice, they use it to protect landed property privileges against reallocation of fiscal pressure to new sources (commerce, manufactures, mobile wealth). They have resources to mount sustained legal and legislative challenge; they can absorb costs and wait out pressure.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, landowning_magistrate_class, agenda_setter,
    organized, generational, mobile, national).

% The royal treasury and its administrators, who must finance state operations (military, bureaucracy, court) and face mounting fiscal pressure from wars, debt service, and institutional expansion. Remonstrance delays and defeats taxing innovations that would reach growing sources of wealth (commerce, chartered companies, urban centers). The Crown experiences remonstrance as obstruction of necessary adaptation and capacity to govern.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_fiscal_authority, payer,
    institutional, biographical, constrained, national).

% Merchants, manufacturers, chartered-company shareholders, and urban financiers whose wealth and economic dynamism lie outside traditional landed property and local magisterial control. They benefit from Crown policies that enable trade networks and manufacturing (infrastructure, monopoly grants, naval protection) but are excluded from remonstrance processes dominated by landed magistrates. When the Crown attempts to tax commercial activity or shift fiscal burden from land to commerce, remonstrance blocks the adjustment—commercial interests absorb the blocked taxation or pay it indirectly through reduced credit access and higher borrowing costs.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, commercial_urban_interests, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, commercial_urban_interests, excluded).

% Titled landowners whose estates and local jurisdiction are anchored in the same property base the magistracy defends. They are inseparable from the magistrate class structurally; remonstrance protects their fiscal privileges as part of protecting the landed order. They benefit from the blocking of fiscal innovation without having to mount every remonstrance themselves.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, hereditary_nobility, beneficiary,
    organized, generational, mobile, national).

% Estates, parlements, and representative bodies dominated by landed interests and magistrates. They are the institutional vessels through which remonstrance operates—they formulate and transmit the legal challenge to Crown edicts. They benefit from the legitimacy remonstrance grants to their intervention in fiscal authority; their institutional prestige depends on the credibility of the remonstrance claim.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_assemblies, beneficiary,
    organized, generational, mobile, regional).

% Royal jurists and constitutional lawyers who theorize the Crown's authority to override or supersede remonstrance, or who defend remonstrance as a constitutional limit on Crown power. They produce the competing legal framings—whether remonstrance is constitutional duty or obstruction—and their analysis feeds the legitimacy contest.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_legal_advisors, observer,
    moderate, biographical, analytical, national).

% Commoners, tenant farmers, urban laborers, and non-propertied populations who have no standing in remonstrance processes and no institutional voice in the fiscal struggle. They experience the fiscal consequences of blocked Crown taxation (which persists as alternative levies, labor obligations, or price pressures) without the capacity to remonstrate or participate in the outcome.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, popular_urban_and_rural_commons, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, landowning_magistrate_class).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Remonstrance provides a mechanism by which established regional and property-holding interests can assert legal objections to central fiscal innovation, anchoring local authority and fiscal autonomy against arbitrary Crown reallocation.
% TRANSFER_FUNCTION: Blocks or delays the transfer of fiscal burden from landed property to emerging sources of wealth (commerce, manufactures, chartered entities), preserving the tax burden on the Crown's capacity to innovate revenue sources and leaving the landed interest's relative fiscal advantage intact.
% ABSENT_VOICES: Commercial interests excluded from remonstrance processes, and popular commoners with no property standing—both of whom would object that remonstrance protects privilege at their expense, that it blocks fiscal modernization necessary for capacity building, and that it substitutes magistrate particularism for universal rule.
% DISAPPEARANCE_RATIONALE: If remonstrance right and its enforcement machinery disappeared, Crown fiscal authority would rapidly shift the tax burden from fixed land to mobile commerce and manufactures; provinces would lose fiscal leverage against Crown directives; magistrate class political standing would erode absent the legitimacy remonstrance provides; commercial wealth would become fiscally dominant relative to landed property.
% FOUNDING_PROBLEM: Medieval and early-modern kingdoms developed provincial and local authority structures (estates, parlements, magistracies) as counterweights to arbitrary monarchy; remonstrance embodied the claim that these bodies held constitutional duty to block or delay edicts that violated ancient privileges and usages of their constituencies.
% FOUNDING_PROBLEM_CORROBORATION: Crown legal reformers and fiscal historians document that by 1700–1789 the founding problem (arbitrary monarchy overriding property rights) no longer characterized Crown behavior—fiscal pressure came from legitimate state expenses (wars, debt service, institutional necessity), not from arbitrary action. Magistrate and noble interests continue to attest the founding problem is live, but outside corroborators (comparative historians studying jurisdictions that abolished remonstrance, legislative reformers proposing fiscal modernization) attest the founding problem is historical and remonstrance has become privilege protection. No corroborating source outside the benefiting parties (magistrates, nobles) attests the founding problem remains live; only the parties extracting benefit claim it does.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.42→0.68) because remonstrance operates as a veto over fiscal innovation that would dilute landed privilege; as fiscal pressure accumulated, the blocking became more valuable and more extracted. Suppression is high and rising (0.48→0.71) because the enforcement of remonstrance depends on active exclusion of alternative voices (commercial, popular) and on the magistracy's organized capacity to mount sustained legal and political challenge; Crown attempts to override or supersede remonstrance trigger institutional conflict. Theater is moderate and rising (0.25→0.42) because the constitutional narrative (ancient liberty, property right) persists and provides the legal vessel for the blocking; but over time, the disproportion between the stated justification (constitutional restraint) and the functional effect (preserving fiscal privilege against modernization) becomes visible—the justification becomes increasingly theatrical. Accessibility collapse is moderate (0.52) because alternatives to remonstrance exist but are systematically devalued: the Crown could attempt to bypass magistrates and address commercial interests directly, but the institutional embedding of remonstrance makes that costly; commercial interests could attempt to organize counter-remonstrance, but they lack the constitutional standing and institutional infrastructure magistrates possess. Resistance is moderate (0.58) because the Crown and commercial interests do resist—they mount legal challenges, attempt to negotiate, sponsor competing constitutional theories—but the landed institutional base and the legitimacy of the constitutional claim give remonstrance persistent power. All measurements share one time grid (endpoints 1600, 1789) so every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The magistrate and noble seats should compute as beneficiary/rope from their own framing: they see themselves as constitutional guardians restraining arbitrary power, providing coordination benefit (stable property law, predictable local rule) at the cost of some central fiscal constraint. The Crown seat should compute as victim/snare from its framing: the constraint blocks legitimate fiscal innovation, protects unearned privilege, operates by excluding competing voices, and persists through organized institutional power rather than consensus or natural law. Commercial and popular seats should compute as victims/snare from their framing: they are excluded and trapped, absorbing the cost of blocked modernization without voice in the outcome. The engine computes these divergences from the structural data; the authored claim (snare) reflects the Crown/reformer reading, which becomes dominant by 1789.
 *
 * DIRECTIONALITY LOGIC:
 *   The landowning magistrate class is the structural beneficiary (d near 0.0): they collect the benefit of blocked taxation (their relative fiscal advantage is preserved), they set the agenda (formulate and transmit remonstrance), they have mobile exit options (they can exit magnate politics but retain property standing), and they are at organized power level (capable of sustained institutional challenge). The Crown fiscal authority is the primary victim (d near 1.0): they face systematic blocking of revenue innovations, they are constrained by law and institutional pressure they did not authorize, they must negotiate around remonstrance rather than override it directly, and they are institutional power facing organized resistance. Commercial interests are secondary victims (d near 0.8): they are excluded from remonstrance process, they absorb either blocked taxation or reduced fiscal capacity for public goods they benefit from, they are powerful but lack constitutional standing to remonstrate, and they have constrained exit (they cannot leave the kingdom but can invest elsewhere). Popular commoners are trapped victims (d near 0.95): they are powerless, they have no remonstrance capacity, they experience fiscal consequences without voice, and they are identity-locked to the kingdom by birth and dependency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary monarchy overriding provincial and property rights) was live in 1600 and provided genuine constitutional restraint function. By 1700, the problem is contested: the Crown's fiscal pressure is largely legitimate (wars, debt, institutional necessity) and remonstrance blocks necessary adaptation rather than arbitrary action. By 1789, the founding problem is dead for most observers: monarchy has not become more arbitrary, but remonstrance has become more extractive—it protects privilege, not liberty. The classification prevents misreading remonstrance as pure coordination (rope) by naming the victim set (Crown, commercial, commoners) and the extraction mechanism (blocking fiscal innovation); it also prevents misreading as pure structure (mountain) by showing high suppression and active enforcement. The mandatrophy lies in the gap between founding justification (constitutional restraint) and persistent function (privilege protection): the founding problem is dead but the constraint persists because it collects rents for the magistracy and provides legitimacy for their obstruction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_supersession,
    'Is the founding problem (arbitrary monarchy threatening property rights) genuinely superseded by 1700–1789, or does it persist and remonstrance continues to serve essential restraint?',
    'Comparative analysis of Crown behavior in jurisdictions that abolished remonstrance (outcomes re: property security, fiscal innovation, arbitrary action). Cross-referencing magistrate and Crown legal briefs against historical evidence of threat/outcome.',
    'If the founding problem is dead, remonstrance is mandatrophy—privilege protection masquerading as constitutional duty. If the problem persists, remonstrance is legitimate restraint on Crown overreach. Classification hinges on this: dead problem + persistent constraint = piton/snare; live problem + persistent constraint = tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_supersession, empirical, 'Whether the founding problem has been superseded or remains live.').

omega_variable(
    alternative_coordination_separability,
    'Are the coordination functions remonstrance provides (local fiscal autonomy, property-rights restraint on monarchy, provincial voice in central directives) separable from the extraction mechanism (blocking fiscal innovation that would dilute privilege)?',
    'Natural experiment from reforms that retained local representation but removed veto power over Crown taxation (e.g., participatory budget input without blocking capacity). Comparative outcomes on property security, fiscal adaptability, and institutional legitimacy.',
    'If separable, the extraction is pure veto rent and classification stands as snare. If inseparable, some of the measured suppression and extraction are costs of the coordination function itself, pushing classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_separability, conceptual, 'Whether coordination and extraction functions are structurally separable in remonstrance architecture.').

omega_variable(
    reading_framework_contest,
    'Is the Crown reading''s framing of remonstrance as illegitimate minoritarian veto the correct constitutional reading, or is the magistrate reading''s framing as fundamental liberty protection the correct one?',
    'This is a preference/conceptual omega, not empirical. The resolution requires commitment to a theory of constitutional authority: whether property-holding local actors have constitutional standing to veto central directives, or whether centralized fiscal authority is constitutionally sovereign. No fact about what remonstrance does can resolve this; the dispute is about what remonstrance should be.',
    'The reading chosen determines whether the constraint is classified as legitimate (magistrate reading: rope or balanced tangled_rope) or illegitimate (Crown reading: snare). The ε stays the same; the classification frame shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framework_contest, preference, 'Which reading of the remonstrance-authority kernel is normatively correct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1600, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1600, remonstrance_authority__crown_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement_basis(remo_tr_t1600, observed).
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__crown_reading, theater_ratio, 1650, 0.28).
narrative_ontology:measurement_basis(remo_tr_t1650, observed).
narrative_ontology:measurement(remo_tr_t1700, remonstrance_authority__crown_reading, theater_ratio, 1700, 0.32).
narrative_ontology:measurement_basis(remo_tr_t1700, observed).
narrative_ontology:measurement(remo_tr_t1740, remonstrance_authority__crown_reading, theater_ratio, 1740, 0.38).
narrative_ontology:measurement_basis(remo_tr_t1740, observed).
narrative_ontology:measurement(remo_tr_t1770, remonstrance_authority__crown_reading, theater_ratio, 1770, 0.41).
narrative_ontology:measurement_basis(remo_tr_t1770, observed).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__crown_reading, theater_ratio, 1789, 0.42).
narrative_ontology:measurement_basis(remo_tr_t1789, observed).

% Extraction over time
narrative_ontology:measurement(remo_be_t1600, remonstrance_authority__crown_reading, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement_basis(remo_be_t1600, observed).
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__crown_reading, base_extractiveness, 1650, 0.48).
narrative_ontology:measurement_basis(remo_be_t1650, observed).
narrative_ontology:measurement(remo_be_t1700, remonstrance_authority__crown_reading, base_extractiveness, 1700, 0.56).
narrative_ontology:measurement_basis(remo_be_t1700, observed).
narrative_ontology:measurement(remo_be_t1740, remonstrance_authority__crown_reading, base_extractiveness, 1740, 0.64).
narrative_ontology:measurement_basis(remo_be_t1740, observed).
narrative_ontology:measurement(remo_be_t1770, remonstrance_authority__crown_reading, base_extractiveness, 1770, 0.66).
narrative_ontology:measurement_basis(remo_be_t1770, observed).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__crown_reading, base_extractiveness, 1789, 0.68).
narrative_ontology:measurement_basis(remo_be_t1789, observed).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1600, remonstrance_authority__crown_reading, suppression_requirement, 1600, 0.48).
narrative_ontology:measurement_basis(remo_su_t1600, observed).
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__crown_reading, suppression_requirement, 1650, 0.54).
narrative_ontology:measurement_basis(remo_su_t1650, observed).
narrative_ontology:measurement(remo_su_t1700, remonstrance_authority__crown_reading, suppression_requirement, 1700, 0.61).
narrative_ontology:measurement_basis(remo_su_t1700, observed).
narrative_ontology:measurement(remo_su_t1740, remonstrance_authority__crown_reading, suppression_requirement, 1740, 0.67).
narrative_ontology:measurement_basis(remo_su_t1740, observed).
narrative_ontology:measurement(remo_su_t1770, remonstrance_authority__crown_reading, suppression_requirement, 1770, 0.7).
narrative_ontology:measurement_basis(remo_su_t1770, observed).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__crown_reading, suppression_requirement, 1789, 0.71).
narrative_ontology:measurement_basis(remo_su_t1789, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__crown_reading, 0.12).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% The remonstrance-authority kernel decomposes into two constraint stories: the Crown reading (this story) interprets remonstrance as extractive veto protecting landed privilege; the magistrate reading interprets remonstrance as constitutional restraint protecting ancient liberties. The two readings have substantially different ε values (Crown reading: high extraction; magistrate reading: moderate extraction with coordination component) and different beneficiary/victim structures. Both readings are live historical claims held by different parties in the same period; neither forecloses the other logically, though they compete for institutional authority. The network link allows corpus analysis to model the readings as structurally interdependent: the Crown reading's case depends on demonstrating the magistrate reading is false (that remonstrance is privilege protection, not liberty protection); the magistrate reading's case depends on demonstrating the Crown reading mischaracterizes the constraint's function. Neither reading is authoritative; both are data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
