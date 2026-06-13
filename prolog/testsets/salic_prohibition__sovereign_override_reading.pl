% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law as Sovereign-Revocable Succession Prohibition
 *   domain: constitutional/dynastic/political
 *
 * SUMMARY:
 *   The Salic Law prohibition on female succession is read under this
 *   constraint as a revocable positive law, enacted by sovereign authority
 *   and subject to legislative override via instruments like the Pragmatic
 *   Sanction. The sovereign may alter the rule unilaterally to secure
 *   dynastic continuity when no male heir exists; challengers who contest
 *   female succession are framed as rebels against legitimate sovereign
 *   authority, not defenders of inviolable law. The coordination function is
 *   genuine (concentrating succession, reducing fragmentation), but the
 *   constraint also extracts from excluded parties (female claimants,
 *   collateral males, feudal nobility bearing war costs). Measurement series
 *   document a plateau: extractiveness rises steeply in the interval's first
 *   half (as Pragmatic Sanctions become more frequent and more contested)
 *   then stabilizes; theater rises more gradually as the rule's enforcement
 *   machinery becomes more visible (ceremonial validation of female
 *   succession, spectacular punishment of rebellious collateral claimants).
 *
 * KEY AGENTS:
 *   - reigning_male_monarch: Agenda setter; administers and enforces the rule; can unilaterally revoke via Pragmatic Sanction.
 *   - senior_male_lineage_holders: Primary beneficiaries; secure enhanced inheritance probability; can exit via cognatic rival powers.
 *   - female_succession_claimants: Victims; barred from succession, must petition for exception; exit options identity-locked (succession is their identity claim).
 *   - collateral_male_lines: Victims when overridden by female claimants; formal Salic right displaced by sovereign act; constrained exit (can rebel, but rebellion is criminalized).
 *   - ambitious_rival_powers: Excluded; hold cognatic systems; sponsor collateral male challenges to destabilize Pragmatic Sanctions.
 *   - feudal_nobility: Payers; bear military cost of enforcing Pragmatic Sanctions; constrained exit (feudal oath binds them).
 *   - ecclesiastical_authority: Observer and secondary beneficiary; legitimizes sovereign overrides; gains influence as validator.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.71).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law as Sovereign-Revocable Succession Prohibition").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/dynastic/political").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '1b194b5d-f0e2-4b18-92e8-22d9b2d3e805').
narrative_ontology:cs_kernel_codification('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', fixed_text).
narrative_ontology:cs_authority_grounding('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', lineage).
narrative_ontology:cs_interpretation_layer_present('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805').
narrative_ontology:cs_reading_relation('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', salic_prohibition__cognatic_reversion_reading, influences).
narrative_ontology:cs_axiom('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', foundational, sovereign_legislative_supremacy_over_succession).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy_over_succession, holdable).
narrative_ontology:cs_axiom_grounding('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', sovereign_legislative_supremacy_over_succession, deontological).
narrative_ontology:cs_axiom('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', secondary, pragmatic_sanction_as_legitimate_override_mechanism).
narrative_ontology:cs_axiom_status(pragmatic_sanction_as_legitimate_override_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', pragmatic_sanction_as_legitimate_override_mechanism, conventional).
narrative_ontology:cs_reference_frame('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', salic_law_as_sovereign_revocable).
narrative_ontology:cs_drift_state('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', post_spanish_succession_war, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b194b5d-f0e2-4b18-92e8-22d9b2d3e805', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, senior_male_lineage_holders).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_succession_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, collateral_male_lines).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, feudal_nobility).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, sovereign_legislative_supremacy).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, pragmatic_sanction_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and enforces Salic prohibition on female succession; administers the rule through inheritance law and court decisions. May unilaterally revoke via Pragmatic Sanction or similar legislative act to secure dynastic continuity when no male heir exists. Bears the cost of warfare when challengers contest female succession as illegitimate.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_male_monarch, agenda_setter,
    institutional, biographical, trapped, national).

% Inherit preferentially under Salic rule; secure their claim against female relatives and distant male collaterals. The prohibition concentrates succession wealth within a narrower male line, increasing each male holder's inheritance probability. Can exit by challenging the rule in competing jurisdictions or by dynastic alliance with female-succession powers (cognatic rivals).
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, senior_male_lineage_holders, beneficiary,
    powerful, generational, mobile, national).

% Barred from succession under Salic law despite blood kinship to the throne. Must petition the monarch for a Pragmatic Sanction to claim legitimately; their succession is framed as an extraordinary exception to the rule, not a right. Their children retain the stigma of female-line inheritance, disadvantaging succession disputes in subsequent generations.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_succession_claimants, payer,
    moderate, biographical, identity_locked, national).

% Distant from the senior line but eligible under Salic rules; displaced by female claimants when a Pragmatic Sanction is issued. Their claim is formal (Salic law backs them) but de facto revoked by sovereign override. They are targets of enforcement when they contest female succession as illegitimate and mount dynastic rebellion.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, collateral_male_lines, payer,
    moderate, biographical, constrained, national).

% Hold territories with cognatic succession rules (female succession permissible); threatened by the Salic model's exclusion of their female-line alliance candidates. Are contractually and militarily barred from direct claim but sponsor male challengers and collateral claimants to contest Pragmatic Sanctions and destabilize female-succession settlements.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, ambitious_rival_powers, excluded,
    institutional, generational, trapped, continental).

% Interprets dynastic legitimacy claims from doctrinal grounds (divine law, natural law, papal authority over succession disputes). Validates or contests Pragmatic Sanctions through doctrinal pronouncements; benefits indirectly from the sovereign's need for ecclesiastical endorsement to legitimize female-succession overrides.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, ecclesiastical_authority, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, ecclesiastical_authority, beneficiary).

% Bears the military cost of defending Pragmatic Sanctions against collateral-male challenges and rival-power-sponsored rebellions. Their obligation to support the sovereign's succession choice is mandatory under feudal law; they cannot exit without breaking their oath. Disputes over female succession create extended warfare they must fund.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, feudal_nobility, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, senior_male_lineage_holders).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a singular, predictable succession rule that prevents the fragmentation of dynastic territories among competing claimants (senior male line only, until sovereign override when childless). Concentrates sovereignty in a clear primary line and eliminates disputes arising from multiple descent paths.
% TRANSFER_FUNCTION: Moves succession rights and the dynastic estate from female-line and collateral-male claimants to the senior male line (or to a female claimant designated by sovereign Pragmatic Sanction). The cost to excluded parties is loss of potential inheritance and diminished marriage-alliance value in international dynastic negotiations.
% ABSENT_VOICES: Rival powers holding cognatic succession systems are structurally excluded from the legitimacy conversation within the framework; they would argue that female succession is natural law (their own system vindicates it) and that Salic exclusion is a Frankish anachronism inapplicable outside its origin territory. Peasant populations bear the warfare cost of succession disputes but are never consulted on the rule.
% DISAPPEARANCE_RATIONALE: If Salic prohibition and its enforcement vanished, female-line and collateral-male claims would immediately activate; succession disputes would widen to include cognatic rival powers; warfare over inheritance would intensify and redraw territorial borders within a generation.
% FOUNDING_PROBLEM: Early medieval Frankish succession produced competing heirs from multiple lineages; fragmentation of the estate among all possible claimants destabilized the kingdom and invited external conquest. A rule excluding females and prioritizing the senior male line concentrated power and reduced internal dispute.
% FOUNDING_PROBLEM_CORROBORATION: Senior male lineage holders and monarchs attest the founding problem is still live, citing the chaos of multi-claimant succession (War of the Spanish Succession, 1701–1714, fought partly over the Salic rule). Ecclesiastical historians and cognatic rival powers attest the founding problem is solved — kingdoms with female and cognatic succession rules are stable and prosperous; Salic law persists as rent extraction, not coordination necessity. The shift in testimony comes from non-benefiting seats; legislative records and contemporary legal scholarship document the reading that Salic law is revocable positive law, not immutable mandate.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at interval end, reflecting the constraint's dual nature: genuine coordination (succession clarity) coexists with extraction from excluded parties. The upward trajectory (0.48→0.62) captures the intensification of Pragmatic Sanction usage in the later interval, as male-heir childlessness became more common and female succession more frequently invoked. Suppression is higher (0.71) because the rule's persistence depends on active military enforcement against collateral-male rebellions and rival-power challenges, not on voluntary participation by excluded parties. Theater is moderate (0.48) because the rule retains a functional core (preventing fragmentation) while its enforcement increasingly becomes ceremonial legitimation of exceptions rather than application of a fixed principle. The measurement series share one time grid: all three metrics are authored at points t∈{0,4,8,12,16,20,24,28}.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (reigning monarch) computes the constraint as rope: genuine coordination mechanism, legitimate sovereign authority, voluntary compliance from beneficiaries. Female claimants and collateral males compute it as snare: coercive exclusion, suppressed alternatives (cognatic succession is viable but forbidden), extraction via inheritance denial. The engine computes these divergences from the structural data independently; the authored claim (tangled_rope) sits between and documents the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary (senior male line: low d, subsidized by the rule) and victim (female claimants, collateral males: high d, extracted from). Senior males have arbitrage-grade exit (can ally with cognatic powers, can accept female succession voluntarily), pushing their d downward. Female claimants are identity-locked (succession is their identity claim, exit means abandoning their stake in the throne entirely), pushing their d upward. Feudal nobility are trapped (oath binds them), moderately powered, and bear extraction indirectly (war costs), placing them high-d despite not being named as direct victims. The reigning monarch is analytically the most powerful but also trapped (must maintain the rule to defend male succession while being able to revoke it — a paradox resolved by framing revocation as extraordinary sovereign prerogative, not routine law revision).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophy. Its founding problem (multi-claimant fragmentation) is DEAD in the 18th century onward, yet the constraint persists and is actively maintained. However, the constraint is NOT purely theatrical (theater_ratio is 0.48, not dominant) because the coordination function remains operative: the rule continues to clarify succession and reduce internal dispute, even as female succession becomes more frequent. Mandatrophy would require the constraint to be maintained wholly by inertia and performance despite zero functional value; this constraint retains functional value (succession clarity) while also extracting (inheritance denial, war costs) — it is a tangled rope whose founding problem has shifted, not a piton. The omega variable documents whether the constraint would persist if the founding problem were truly dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_death_vs_persistence,
    'If the founding problem (fragmentation via multi-claimant succession) is dead — kingdoms with cognatic succession are stable — why does Salic prohibition persist and remain actively enforced rather than fade into disuse?',
    'Comparative institutional analysis: compare succession disputes and internal warfare rates in Salic jurisdictions vs. cognatic jurisdictions over the same periods. If Salic causes fewer disputes, the founding problem is live; if cognatic jurisdictions show equal or lower dispute rates, the founding problem is dead and the constraint is maintained for rent extraction.',
    'If the founding problem is dead, the constraint reclassifies from tangled_rope (with live coordination function) toward piton (coordination atrophied, extraction persists via inertia and performance). The institutional analysis would determine whether male-line exclusion genuinely solves succession clarity or whether that clarity is independent of the gender rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_death_vs_persistence, empirical, 'Whether male-line concentration actually prevents succession fragmentation or whether fragmentation is a function of the number of claimants regardless of gender rules.').

omega_variable(
    identity_lock_mechanism_for_female_claimants,
    'Is female claimants'' identity-locked exit status accurate, or is their lock more constrained than identity-locked suggests? What happens to female claimants who renounce succession to marry cognatic powers and exit the system entirely?',
    'Historical case study of female claimants who renounced succession or married abroad: did they retain political agency and influence, or were they stripped of it? Did their renunciation constitute a real exit or merely a different form of powerlessness?',
    'If female claimants can exit via renunciation and retain agency, their exit is more mobile than identity-locked; their directionality would be lower (less fully targets), lowering their extraction χ. If renunciation strips them of all agency (a common historical pattern), their lock is tighter and their extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_female_claimants, empirical, 'Whether female succession claimants can exit the constraint meaningfully or whether renunciation is a form of entrapment.').

omega_variable(
    sovereign_override_as_legitimacy_cover,
    'Is the Pragmatic Sanction a genuine exercise of sovereign legislative authority, or is it a theatrical cover story that allows the sovereign to override Salic law while maintaining the fiction that Salic law is immutable?',
    'Doctrinal analysis of how Pragmatic Sanctions are justified in contemporary legal texts and ecclesiastical pronouncements. If justified as ''extraordinary exception to immutable law'' vs. ''legitimate exercise of sovereign power to revise law'', the framing differs; if both framings are used simultaneously, the constraint is performing a legitimacy bridge between two incompatible readings.',
    'If Pragmatic Sanction is genuine legislative authority, the constraint embodies a real mechanism for law revision and the monarchy retains principled discretion. If it is a cover story, the constraint is more snare-like (suppression, alternatives concealed) and the theater ratio should be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereign_override_as_legitimacy_cover, conceptual, 'Whether the sovereign''s override mechanism is a principled exercise of legislative authority or a theatrical legitimation of de facto rule-breaking.').

omega_variable(
    reading_vs_immutable_mandate_contest,
    'Why would collateral male claimants and rival powers prefer the immutable_mandate_reading over this sovereign_override_reading? What structural advantage does immutability give to challengers?',
    'Doctrinal and military history: when collateral males rebel against Pragmatic Sanctions, what legitimacy claim do they invoke? Do they argue Salic law is immutable (the immutable_mandate frame), or do they argue the sovereign lacked authority to override (a different legitimacy claim)? What ecclesiastical support does each reading receive?',
    'If challengers invoke immutable_mandate more often than sovereign_override objections, it suggests immutable_mandate is strategically advantageous (makes the sovereign''s override look like usurpation, attracts ecclesiastical and rival-power support). The immutable_mandate_reading would then be the dominant contestation frame, not a minority reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_immutable_mandate_contest, empirical, 'Whether the immutable_mandate reading is the dominant challenge to this reading, or whether other framings are more strategically salient.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.31).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t4, salic_prohibition__sovereign_override_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement_basis(sali_tr_t4, observed).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__sovereign_override_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement_basis(sali_tr_t8, observed).
narrative_ontology:measurement(sali_tr_t12, salic_prohibition__sovereign_override_reading, theater_ratio, 12, 0.43).
narrative_ontology:measurement_basis(sali_tr_t12, observed).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__sovereign_override_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement_basis(sali_tr_t16, observed).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__sovereign_override_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(sali_tr_t20, observed).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__sovereign_override_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(sali_tr_t24, observed).
narrative_ontology:measurement(sali_tr_t28, salic_prohibition__sovereign_override_reading, theater_ratio, 28, 0.48).
narrative_ontology:measurement_basis(sali_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t4, salic_prohibition__sovereign_override_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(sali_be_t4, observed).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__sovereign_override_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(sali_be_t8, observed).
narrative_ontology:measurement(sali_be_t12, salic_prohibition__sovereign_override_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement_basis(sali_be_t12, observed).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__sovereign_override_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(sali_be_t16, observed).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__sovereign_override_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(sali_be_t20, observed).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__sovereign_override_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(sali_be_t24, observed).
narrative_ontology:measurement(sali_be_t28, salic_prohibition__sovereign_override_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement_basis(sali_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t4, salic_prohibition__sovereign_override_reading, suppression_requirement, 4, 0.59).
narrative_ontology:measurement_basis(sali_su_t4, observed).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__sovereign_override_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(sali_su_t8, observed).
narrative_ontology:measurement(sali_su_t12, salic_prohibition__sovereign_override_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement_basis(sali_su_t12, observed).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__sovereign_override_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement_basis(sali_su_t16, observed).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__sovereign_override_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(sali_su_t20, observed).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__sovereign_override_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(sali_su_t24, observed).
narrative_ontology:measurement(sali_su_t28, salic_prohibition__sovereign_override_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement_basis(sali_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__sovereign_override_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% The Salic Law constraint decomposes into three structurally distinct constraints under different readings of the kernel. The sovereign_override_reading models Salic law as revocable positive law subject to sovereign legislative authority (ε ≈ 0.62, tangled_rope). The immutable_mandate_reading models it as irrevocable natural/divine law (expected higher extraction, snare-flavored, ε likely > 0.75). The cognatic_reversion_reading models it as a Frankish anachronism never applicable outside Francia (expected lower suppression, rope-flavored, ε likely < 0.45). Each reading instantiates different ε, different beneficiary/victim structures, and different types. The readings coexist in public dispute: different parties hold each simultaneously, and the contest between them IS the constraint's operational reality. Network edges link all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
