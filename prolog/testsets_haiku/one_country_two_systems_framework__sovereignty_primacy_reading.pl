% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems: Sovereignty Primacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   One Country, Two Systems is a constitutional arrangement established in
 *   1997 to govern the handover of Hong Kong from British to Chinese
 *   sovereignty. The framework was designed as a compromise: Hong Kong
 *   retains substantive autonomy, independent courts, and distinctive legal
 *   institutions while acknowledging PRC ultimate state authority. This
 *   constraint story instantiates the SOVEREIGNTY PRIMACY READING of that
 *   framework—the interpretation that treats Hong Kong autonomy as delegated
 *   and revocable, with national security and territorial integrity as trump
 *   cards overriding local authority. This reading became operationalized
 *   through the 2020 National Security Law, which established mainland
 *   enforcement machinery in Hong Kong and redefined political opposition as
 *   potential state crime. The constraint is CLAIMED as Tangled Rope
 *   (coordination of unified state power with delegated local governance) but
 *   the metrics show substantially extractive operation sustained by
 *   enforcement machinery that targets political opposition and civil
 *   liberties. The claim/metric divergence is intentional: the PRC apparatus
 *   frames this reading as natural constitutional supremacy; critics frame it
 *   as constructed extraction riding on a coordination claim. The engine
 *   measures that structural disagreement.
 *
 * KEY AGENTS:
 *   - PRC state apparatus: Sets the reading, enforces it through National Security Law, controls final interpretation—full agenda-setter position with low exit costs (could adopt alternative reading but chooses sovereignty primacy)
 *   - Mainland security establishment: Operates enforcement machinery (liaison offices, prosecution, security doctrine application)—co-agenda-setter bearing enforcement costs but benefiting from expanded jurisdiction
 *   - Hong Kong judiciary: Formally autonomous but functionally constrained on national security matters; identity-locked to rule-of-law principle in conflict with subordination to mainland authority
 *   - Hong Kong civil society: Bears costs through suppressed speech, assembly, journalism; constrained exit (cost of relocation); organized resistance possible but criminalized
 *   - Hong Kong political opposition: Trapped—cannot contest the reading through legal channels (which they're excluded from), cannot exit without cost, cannot stay without accepting the framework
 *   - International observers: Excluded from Hong Kong's constitutional process; dispute the reading on human rights/treaty grounds but have no enforcement mechanism
 *   - Hong Kong business/capital: Benefits from political stability and reduced labor unrest; mobile exit option means this is negotiated partnership rather than pure extraction
 *   - Analytical observer (this story): Maps the structural asymmetries and identifies how enforcement costs are borne differently across stakeholder groups
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.79).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb').
narrative_ontology:cs_kernel_codification('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', fixed_text).
narrative_ontology:cs_authority_grounding('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', extraction).
narrative_ontology:cs_interpretation_layer_present('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb').
narrative_ontology:cs_reading_relation('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', foundational, sovereignty_absolute_and_indivisible).
narrative_ontology:cs_axiom_status(sovereignty_absolute_and_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', sovereignty_absolute_and_indivisible, deontological).
narrative_ontology:cs_axiom('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', foundational, state_security_supremacy_over_local_rights).
narrative_ontology:cs_axiom_status(state_security_supremacy_over_local_rights, holdable).
narrative_ontology:cs_axiom_grounding('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', state_security_supremacy_over_local_rights, empirically_contingent).
narrative_ontology:cs_axiom('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', secondary, two_systems_is_delegated_governance).
narrative_ontology:cs_axiom_status(two_systems_is_delegated_governance, holdable).
narrative_ontology:cs_axiom_grounding('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', two_systems_is_delegated_governance, conventional).
narrative_ontology:cs_reference_frame('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', unified_prc_state_authority_with_delegated_hk_governance).
narrative_ontology:cs_drift_state('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', post_national_security_law_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbb3fbff-9c2f-419f-9c8c-1d1c65118cdb', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_state_apparatus).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_political_opposition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, international_capital_markets).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_academic_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the interpretation of One Country, Two Systems as delegated autonomy revocable when state interests demand it. Established the National Security Law (2020) to operationalize this reading, overriding Hong Kong's Basic Law where conflicts arise. Controls law enforcement, security services, and final constitutional interpretation. Benefits from the framework by consolidating control over a major financial center while maintaining facade of local governance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Operates National Security Law enforcement apparatus, including liaison offices and plainclothes agents in Hong Kong. Prosecutes cases defined by mainland security doctrine. Bears the cost of enforcement infrastructure and political management of international criticism. Receives expanded jurisdiction and perceived threat-suppression benefit from the sovereignty-primacy framing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_establishment, payer).

% Formally retains autonomy but loses substantive independence on national security matters. Judges must defer to mainland security definitions; appeal paths to mainland courts undermine local finality. High-profile judges face career costs for rulings seen as obstructing state interests. Identity fused to rule of law doctrine creates bind: accepting the reading requires abandoning the principle that made Hong Kong courts worth preserving.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    institutional, biographical, identity_locked, local).

% Experiences the framework as coercive law: political speech, assembly, and journalism are subject to mainland security doctrine applied retroactively. NGOs self-censor; activists face prosecution under vague statutes. Geographic proximity to mainland authority (ability to operate in Hong Kong without warrant) collapses alternatives to exit or silence. Constrained by cost of leaving and entanglement of identity with Hong Kong presence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Cannot contest the sovereignty-primacy reading through normal political channels (which are controlled); contesting it publicly triggers prosecution. Exit involves exile, which carries personal and family costs. Operates under knowledge that certain political positions are now legally prohibited, creating anticipatory compliance. Trapped between acceptance of the framework and criminalized resistance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_political_opposition, payer,
    moderate, biographical, trapped, local).

% UN human rights bodies, Western governments, and international bar associations have objected to the reading on grounds that it contradicts Hong Kong's treaty status and international human rights law. Their voices are excluded from Hong Kong's domestic legal process and treated by the state apparatus as foreign interference. Constrained from intervention by diplomatic norms and Hong Kong's formal sovereignty.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_observers, excluded,
    powerful, biographical, constrained, global).

% Receives stability from political suppression: predictability that dissent will not disrupt business, reduced threat of labor unrest, clear alignment with mainland preferences in regulatory matters. Pays modest compliance costs (political self-censorship, accepting mainland security narratives). Differs from opposition in that exit is available—relocation of offices or capital—which makes the constraint a negotiated partnership rather than pure extraction, though the asymmetry remains.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_establishment, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_establishment, payer).

% The framework provides confidence in Hong Kong's continued political control and low likelihood of internal instability that would disrupt finance. This benefits capital seeking safe jurisdiction for Chinese-denominated assets and regional financial services. Pays no direct cost and exit is costless if conditions change elsewhere.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_capital_markets, beneficiary,
    powerful, generational, mobile, global).

% Universities operate under threat of funding withdrawal if they host certain speakers or allow political organizing. Researchers self-censor; academics with international reputations face pressure to relocate or accept mainland oversight. Identity locked to institutional affiliation and scholarly tradition of intellectual freedom creates acute tension.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_academic_community, payer,
    moderate, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_state_apparatus).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified state authority (PRC sovereignty) that can intervene in Hong Kong's local governance when it judges national security or territorial integrity at stake; permits Beijing to override local autonomy through codified supremacy without formally abolishing the two-systems arrangement.
% TRANSFER_FUNCTION: Transfers de facto political authority from Hong Kong institutions (judiciary, legislature, local executives) to mainland security apparatus, extracting compliance with mainland security doctrine; transfers decision-making power over what constitutes legitimate political speech and assembly from Hong Kong courts to mainland authorities.
% ABSENT_VOICES: International human rights bodies, expatriate Hong Kong diaspora, and Taiwan—all of which would dispute the reading—are excluded from Hong Kong's constitutional conversation. They exist outside the jurisdiction and cannot participate in reinterpreting the Basic Law or resisting the National Security Law within Hong Kong's legal system.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned and replaced by autonomy_primacy_reading, Hong Kong civil liberties law would revert to pre-2020 standards; political opposition could organize legally; international arbitration and treaty interpretation would constrain mainland override; the National Security Law would be rewritten or nullified. The financial sector would initially destabilize due to uncertainty, but international capital and expatriate capital would return; brain drain would reverse.
% FOUNDING_PROBLEM: The 1997 handover required a constitutional design that preserved Hong Kong's institutional distinctness from mainland China while acknowledging PRC sovereignty. One Country, Two Systems was meant to solve the problem of how to maintain Hong Kong's autonomy and economic function while transferring formal state authority to Beijing.
% FOUNDING_PROBLEM_CORROBORATION: The PRC state apparatus and mainland security establishment assert the founding problem remains acute: separatism, foreign interference, and institutional drift toward independence all pose threats requiring sovereignty primacy to manage. International observers and Hong Kong civil society assert the problem is substantially solved by the 1997 framework and the constraint now persists as a solution searching for a problem—a vehicle for expanding control. The 2019-2020 protest movement and subsequent crackdowns generated testimony from Hong Kong activists, academics, and judges attesting that the founding problem was well-contained under autonomy-respecting interpretation.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 endpoint) because the constraint transfers political authority and decision-making power from Hong Kong institutions to mainland authorities, with no reciprocal constraint on mainland override. Suppression is also high (0.79) and rising over the interval—measurement points at 0 (before National Security Law, 2017), 3 (NSL passage, 2020), 6 (NSL enforcement ramping, 2021-22), 12 (consolidation, 2023), 18-24 (stabilization, 2024-25) show accelerating enforcement. Theater ratio rises from 0.25 to 0.48, indicating that formal preservation of Hong Kong institutions is increasingly theatrical—the Basic Law's autonomy provisions are re-interpreted away while nominally preserved. Accessibility collapse (0.71) reflects that once the sovereignty-primacy reading is institutionalized through courts and security law, alternatives (autonomy-based interpretation, international arbitration, political negotiation) are dramatically harder to access. Resistance is substantial (0.68) because Hong Kong civil society mounted significant protests (2014-2020), and international observers maintain principled objection, even though that resistance has been suppressed by enforcement. The constraint is tangled because it contains a genuine coordination element (unified state, stable financial system, predictable governance for capital) alongside asymmetric extraction (political authority transferred, civil liberties suppressed, judiciary subordinated). The measurement trajectory shows rent-seeking layering: extraction and suppression both rose over the interval, suggesting the coordination function declined in importance while extraction increased.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC state apparatus seat: the constraint is natural constitutional order—sovereignty is indivisible, delegated autonomy is conditional, national security overrides local preferences. This is coordination (unified state, stable Hong Kong, capital flows). From Hong Kong civil society seat: the constraint is coercive law imposed through enforcement machinery that was not present pre-2020. Autonomy was substantive before; it became theatrical after. From the international observer seat: this is a treaty violation—the Joint Declaration forbade this reading, and it constrains legitimate PRC authority. From the judiciary seat: it is an institutional bind—accepting the reading requires abandoning rule-of-law principle that makes the judiciary worth having. The engine computes these as different seat types: the agenda-setter (PRC) experiences low extractiveness and high coordination benefit; the payer seats (opposition, civil society, judiciary) experience high extractiveness and high suppression; business/international capital experience beneficiary status despite not setting the agenda. This is per-seat classification, not uniform type assignment.
 *
 * DIRECTIONALITY LOGIC:
 *   PRC state apparatus: d ≈ 0.05 (beneficiary). Controls the rule, collects the coordination benefit (unified authority, capital stability), bears minimal exit cost (could adopt alternative reading but chooses not to). Mainland security establishment: d ≈ 0.15 (moderate beneficiary, but secondary). Bears enforcement costs (liaison office operations, prosecution, diplomatic management), benefits from expanded jurisdiction and threat-suppression credit. Hong Kong judiciary: d ≈ 0.88 (target). Pays through subordination, identity-lock conflict, career pressure; exit is costly (relocation damages professional standing). Hong Kong civil society: d ≈ 0.85 (target). Pays through suppressed speech, constrained assembly; exit is costly (relocation, severed social ties). Opposition: d ≈ 0.92 (target). Trapped by exclusion from legal channels, prosecution risk, relocation cost. International observers: d ≈ 0.50 (symmetric, analytical). Neither benefit nor pay directly—constrained from intervening by norms, unable to enforce their reading. Business: d ≈ 0.35 (moderate beneficiary). Benefits from stability without bearing suppression cost (can operate within constraints); mobile exit keeps negotiating power. These derivations follow from: beneficiary/victim declarations (PRC + security apparatus benefit; opposition, judiciary, civil society victimized) + exit options (PRC/business mobile or arbitrage; opposition trapped; judiciary identity-locked; civil society constrained). The autonomy-primacy reading would flip most of these values, which is why the kernel contest matters structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1997): how to preserve Hong Kong's autonomy and economic function while acknowledging PRC sovereignty. Status at interval endpoint (2024-25): contested. PRC security apparatus frames the problem as eternally live—separatism, foreign interference, institutional drift all pose ongoing threats. Hong Kong opposition and international observers frame it as substantially solved by the 1997 design itself—organized challenges emerged in 2014-2020, but they were political opposition and protest, not separatism; Taiwan coexists with PRC without integration pressure; the institutional design proved resilient. The founding_problem_status x disappearance_verdict mismatch diagnosis: founding problem dead + disappearance_verdict world_rearranges = zombie constraint flag. If the problem is dead (institutional separation is stable, no genuine separatist threat), the constraint persists as a vehicle for post-hoc power consolidation. Evidence: R5 corroboration shows no external observers (UNHRC, international law scholars, Hong Kong judiciary) attest that the founding problem is live; only the beneficiary parties (PRC security establishment) assert it. This asymmetry signals mandatrophy. The theater-ratio rise (0.25 to 0.48) supports the diagnosis: as enforcement intensified over 2020-2025, formal preservation of autonomy (Basic Law, separate judiciary, local legislature) became increasingly theatrical—the substance moved to mainland authority while the form remained in Hong Kong institutions. This is classic piton/zombie behavior: the constraint persists through institutional theater and inertia rather than through solving an active problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of One Country, Two Systems correctly interprets the framework: sovereignty primacy (this reading), autonomy primacy, or balanced coexistence?',
    'The contest is fundamentally constitutional and cannot be resolved by external fact-finding. Resolution would require consensus among the parties (PRC, Hong Kong, international community) on a shared interpretation, which is precisely what the contest prevents. Alternatively, behavioral evidence: if mainland authorities consistently override local autonomy on non-security matters, sovereignty_primacy is evidenced; if local courts consistently constrain mainland reach, autonomy_primacy is evidenced.',
    'Each reading produces a different constraint type (snare, rope, tangled_rope), different victim/beneficiary sets, and different epistemic status for the Basic Law and Sino-British Joint Declaration. If autonomy_primacy is the correct reading, this constraint is false—a constructed false summit claiming natural constitutional order. If sovereignty_primacy is correct, the constraint is an accurate description of state supremacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity in the kernel''s core meaning—no external adjudicator exists to settle which reading is correct.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.79) primarily structural (legal barriers, enforcement threat, geographic proximity of mainland authority) or internalized (Hong Kong civil society has accepted the sovereignty-primacy reading and self-regulates)?',
    'Post-constraint relaxation trajectory: if constraints on speech/assembly were formally removed (repeal of National Security Law, restoration of independent judiciary) and suppressive behavior persisted in civil society absent external enforcement, the suppression is internalized. If suppressive behavior ceased upon removal of enforcement machinery, suppression is structural.',
    'Structural suppression can be reduced by legal reform; internalized suppression persists and requires cultural/educational transformation. Misclassifying internalized as structural over-optimizes for formal legal solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is sustained by external coercion or by internalized acceptance.').

omega_variable(
    foundational_problem_obsolescence,
    'Is the 1997 foundational problem (how to preserve Hong Kong''s autonomy while acknowledging PRC sovereignty) still genuinely live, or has it been solved and the constraint now persists as a vehicle for post-hoc power consolidation?',
    'Counterfactual test: if Hong Kong had been allowed to develop under the autonomy_primacy_reading post-1997, would separatism have emerged and posed genuine threat? Historical evidence from the 2014-2020 protest movement shows organized challenge to the political arrangement but not formal separatism; Taiwan, meanwhile, diverges from PRC without integration pressure. Contemporary testimony from mainland security officials frames the founding problem as always-present (''separatism is eternal threat''), but international law scholars and Hong Kong opposition frame it as solved by the institutional design itself.',
    'If the founding problem is dead, the constraint transitions from justified coordination to cover story for extraction—mandatrophy diagnosis. If the problem is live, the constraint remains structurally legitimate (Tangled Rope classification holds). This drives the R5 mismatch analysis that flags zombie constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_obsolescence, empirical, 'Whether the constraint''s founding purpose remains active or has been superseded.').

omega_variable(
    treaty_vs_internal_authority,
    'Does the Sino-British Joint Declaration, registered with the UN, constitute binding international law that constrains PRC interpretation of One Country, Two Systems, or is it a historical document with no enforceable effect on domestic constitutional interpretation?',
    'International Court of Justice advisory opinion or binding arbitration; alternatively, whether UN bodies assert enforcement power. Currently contested between PRC (domestic constitutional supremacy) and international observers (treaty binding). No arbitration path exists because PRC does not accept ICJ jurisdiction on sovereignty matters.',
    'If the Joint Declaration is binding international law, the sovereignty-primacy reading violates a treaty obligation, making the constraint internationally unlawful and potentially subject to sanctions. If it is merely historical, the PRC has unlimited authority to reinterpret One Country, Two Systems. The truth-value of this question determines whether the constraint is legitimate under international law or a violation thereof.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_vs_internal_authority, conceptual, 'The normative status of the Sino-British Joint Declaration in constraining PRC constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(one__tr_t0, observed).
narrative_ontology:measurement(one__tr_t3, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(one__tr_t3, observed).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement_basis(one__tr_t6, observed).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement_basis(one__tr_t12, observed).
narrative_ontology:measurement(one__tr_t18, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 18, 0.47).
narrative_ontology:measurement_basis(one__tr_t18, observed).
narrative_ontology:measurement(one__tr_t24, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(one__tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(one__be_t0, observed).
narrative_ontology:measurement(one__be_t3, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement_basis(one__be_t3, observed).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement_basis(one__be_t6, observed).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement_basis(one__be_t12, observed).
narrative_ontology:measurement(one__be_t18, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 18, 0.81).
narrative_ontology:measurement_basis(one__be_t18, observed).
narrative_ontology:measurement(one__be_t24, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(one__be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(one__su_t0, observed).
narrative_ontology:measurement(one__su_t3, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement_basis(one__su_t3, observed).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 6, 0.71).
narrative_ontology:measurement_basis(one__su_t6, observed).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement_basis(one__su_t12, observed).
narrative_ontology:measurement(one__su_t18, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement_basis(one__su_t18, observed).
narrative_ontology:measurement(one__su_t24, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement_basis(one__su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.18).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_national_security_law_implementation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary_independence_doctrine).

% DUAL FORMULATION NOTE:
% This story is one reading of the one_country_two_systems_framework kernel. The sibling constraints (autonomy_primacy_reading, balanced_coexistence_reading) are other readings of the same kernel text. They do not represent measurement error or observational variation—they represent genuinely different structural claims about what the 1997 framework means. Each reading has its own constraint story with its own ε, beneficiary/victim structure, and type classification. The kernel is the stable commitment; the readings are the contested interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
