% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection — Colorblind Reading (Anticlassification Rule)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the equal_protection_clause
 *   kernel: the colorblind (anticlassification) reading, under which the
 *   Fourteenth Amendment forbids every governmental racial classification and
 *   individuals hold rights independently of group membership. The kernel
 *   decomposes into three structurally distinct constraints — colorblind
 *   (this file), diversity, and remedial — with different beneficiary sets,
 *   different permanence claims, and different epsilon; per the
 *   epsilon-invariance principle they are authored as separate stories linked
 *   through the network. The standing arrangement under contest, and the
 *   referent for every metric below, is the colorblind rule itself as it
 *   operates in contemporary doctrine (post-SFFA), assessed by this reading's
 *   own lights — hence the low authored extractiveness. The claimed type is
 *   authored independently of the metrics: the rule is claimed as
 *   tangled_rope because it couples a genuine coordination function (one
 *   administrable anti-factional line) with asymmetric incidence
 *   (concentrated gains to neutral-competition winners and movement
 *   professionals; concentrated costs to institutions and remedy-seeking
 *   communities) held together by intensive active enforcement. The engine
 *   computes per-seat classifications from the structural data; where a seat
 *   computes differently from this claim, that divergence is the datum. KEY
 *   AGENTS (by structural relationship): - supreme_court_federal_judiciary:
 *   Agenda setter (institutional / identity_locked) — administers the rule
 *   through strict scrutiny; institutional identity fused with the
 *   formal-symmetry principle - racial_minority_citizens: Beneficiary
 *   (moderate / constrained) — shielded from hostile classifications by the
 *   same command that bars race-conscious assistance -
 *   disfavored_admission_applicants: Beneficiary (moderate / constrained) —
 *   concrete gains accrue to individuals who compete well under neutral
 *   criteria - public_universities: Payer (institutional / constrained) —
 *   lost the race-conscious toolkit; absorb compliance and litigation costs -
 *   historically_subordinated_communities: Payer (powerless / trapped) — bear
 *   the cost of the closed remedy; no exit from the legal order -
 *   colorblind_movement_lawyers: Beneficiary (organized / identity_locked) —
 *   professional community collecting doctrinal vindication -
 *   remedial_reading_advocates: Excluded (organized / trapped) — hold the
 *   rival reading; no seat in operative doctrine - legal_academy_observers:
 *   Observer (analytical / analytical) — maps the gap between formal symmetry
 *   and measured outcomes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.14).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.82).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection — Colorblind Reading (Anticlassification Rule)").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'a095d650-7871-4684-b54c-96e1ff65a79d').
narrative_ontology:cs_kernel_codification('a095d650-7871-4684-b54c-96e1ff65a79d', fixed_text).
narrative_ontology:cs_authority_grounding('a095d650-7871-4684-b54c-96e1ff65a79d', lineage).
narrative_ontology:cs_interpretation_layer_present('a095d650-7871-4684-b54c-96e1ff65a79d').
narrative_ontology:cs_reading_relation('a095d650-7871-4684-b54c-96e1ff65a79d', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('a095d650-7871-4684-b54c-96e1ff65a79d', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('a095d650-7871-4684-b54c-96e1ff65a79d', foundational, individual_rights_precede_group_membership).
narrative_ontology:cs_axiom_status(individual_rights_precede_group_membership, holdable).
narrative_ontology:cs_axiom_grounding('a095d650-7871-4684-b54c-96e1ff65a79d', individual_rights_precede_group_membership, deontological).
narrative_ontology:cs_axiom('a095d650-7871-4684-b54c-96e1ff65a79d', secondary, classification_symmetry_doctrine).
narrative_ontology:cs_axiom_status(classification_symmetry_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a095d650-7871-4684-b54c-96e1ff65a79d', classification_symmetry_doctrine, deontological).
narrative_ontology:cs_reference_frame('a095d650-7871-4684-b54c-96e1ff65a79d', anticlassification_founders_rule).
narrative_ontology:cs_drift_state('a095d650-7871-4684-b54c-96e1ff65a79d', contemporary_post_sffa, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('a095d650-7871-4684-b54c-96e1ff65a79d', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, racial_minority_citizens).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, disfavored_admission_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, colorblind_movement_lawyers).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, public_universities).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, historically_subordinated_communities).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, anticlassification_principle).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, harlan_plessy_dissent).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, strict_scrutiny_fatal_in_fact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reviews every governmental use of racial categories under strict scrutiny and strikes down programs that fail it. Since the late 1970s the doctrine has hardened from case-by-case balancing toward a near-categorical rule, completed in the 2023 admissions decision. The institution's legitimacy story is now bound up with the formal-symmetry principle; its members reach the bench through a selection pipeline that screens for commitment to it, and changing course would require the institution to repudiate its own recent landmark rulings.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, supreme_court_federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Stand on both sides of the rule in practice. They are shielded from hostile state action — segregated schools, exclusionary ordinances, facially discriminatory statutes — by the same command that bars race-conscious assistance. Formal protection arrives automatically; targeted help must route through race-neutral proxies such as income and geography, which reach them less precisely.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, racial_minority_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Compete for university admission, public contracts, and legislative representation under rules that forbid their race being counted against them. Their organized litigation vehicle won the 2023 ruling; the concrete gain — seats, contracts, and districts allocated without racial discount — lands on individuals who compete well under neutral criteria.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, disfavored_admission_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Lost the ability to consider race in admissions after decades of relying on it. They invest in race-neutral substitutes — essays, adversity measures, targeted recruiting — that recover part but not all of prior diversity levels, and they face continuing litigation exposure over how far proxy engineering may go before it counts as a forbidden classification in disguise.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, public_universities, payer,
    institutional, generational, constrained, national).

% Bear the cost of the closed remedy: these are the communities race-conscious programs were built to reach. Their access routes now run through race-neutral instruments that dilute with distance from the targeted need, and their organizations' traditional legal strategy — litigation demanding race-targeted relief — has been foreclosed by the same rulings that protect them from hostile classifications. There is no exit from the legal order that binds them.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historically_subordinated_communities, payer,
    powerless, generational, trapped, national).

% A professional community of jurists, litigators, and academics whose careers and institutions were built around advancing the formal-symmetry reading. They staff the appointment pipeline, write the briefs, and collect doctrinal vindication as their positions convert into majority opinions. Their professional identity is fused with the principle; abandoning it would dissolve the network's organizing purpose.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, colorblind_movement_lawyers, beneficiary,
    organized, generational, identity_locked, national).

% Scholars, practitioners, and community organizations holding that equal protection requires race-conscious remediation of historical subordination. They argue in dissents, law reviews, and state legislatures but hold no seat in the operative doctrine; their path back runs through appointment turnover or constitutional amendment, both multi-decade projects.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, remedial_reading_advocates, excluded,
    organized, generational, trapped, national).

% Constitutional scholars across the spectrum who map the doctrine's evolution, model its effects, and testify in litigation. They hold no stake in the rule's survival and analyze the gap between its formal symmetry and measured racial outcomes.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, legal_academy_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__colorblind_reading, disfavored_admission_applicants).
narrative_ontology:fixing_cost_class(equal_protection_clause__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every governmental actor a single administrable line — race may never be the basis of state action — replacing case-by-case racial balancing. It addresses a real collective-action problem: without the line, each legislature, board, and agency faces standing pressure to allocate by racial coalition, and every racial faction gains an incentive to capture state machinery. The bright line also lowers adjudication costs: courts police one question instead of running open-ended balancing.
% TRANSFER_FUNCTION: Moves decision authority over race-conscious policy from legislatures, universities, and agencies to the courts, and reallocates the underlying goods — admissions seats, contracts, districts, program eligibility — from race-conscious allocation to race-neutral competition, where they accrue to individuals who prevail under neutral criteria.
% ABSENT_VOICES: The remedial-reading constituency sits outside the operative doctrine: the communities race-conscious programs served, and the legal organizations that represented them, appear in dissents and scholarship but not in the governing rule. Inside the institution, the dissenting justices articulate the objection; outside it, the excluded seats' path back runs through appointment turnover or amendment.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reopen the entire policy space: universities would reinstate race-conscious admissions within an admissions cycle, state statutory bans would face immediate repeal campaigns, federal contracting set-asides would return, and every pending challenge to race-neutral proxy schemes would invert. Thousands of institutional arrangements built around the rule's existence would reorganize.
% FOUNDING_PROBLEM: State-enforced racial caste: the slave codes, Black Codes, and Jim Crow segregation that made race a legal category of subordination. Harlan's dissent articulated the founding commitment — the Constitution neither knows nor tolerates classes among citizens — and the modern reading re-founded it against benign quotas and then against all race-conscious state action.
% FOUNDING_PROBLEM_CORROBORATION: Historians and the judicial record corroborate the founding problem's original reality — the Jim Crow statutes are uncontested facts outside any party's control. On current status, corroboration splits outside the beneficiary set: the social-science literature on persistent disparities and the dissenting justices attest continued structural subordination, while the colorblind movement's own briefs concede the historical problem and attest it is solved. No source outside the contest adjudicates between them.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.14, reading-indexed over the fixed referent of the colorblind rule itself): formal rule application transfers little directly, and the gentle rise across the interval tracks widening incidence as the rule extended from invidious to benign classifications (Croson, Adarand, Parents Involved, SFFA), imposing opportunity costs on more policy spaces. Suppression is high (0.82) and is the interval's central dynamic: enforcement machinery matured from rare fatal applications (Fullilove-era deference) through scrutiny 'fatal in fact' (Croson, Adarand) to categorical closure of admissions uses (SFFA) — the suppression_requirement series is authored because the story specifically traces enforcement intensification, not merely extraction drift. Theater stays low (0.18): the rule's activity is overwhelmingly functional (striking down, enjoining, remanding), with a modest performative layer where formal-symmetry rhetoric outruns measured outcome change. Accessibility_collapse is 0.62: within the doctrine's binding force, the race-conscious alternative collapses almost completely, but interpretive alternatives (sibling readings, amendment, appointment-driven reversal) remain live, which keeps it well below mountain-range values. Resistance is high (0.72): four-justice dissents, sustained academic opposition, state ballot fights, and proxy-engineering litigation meet the rule continuously. All three series run on one shared eight-point grid (1978-2025) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute opposite types from one structure. From the agenda-setter seat, the arrangement is the rule of law itself — the Court experiences the constraint as its own identity and craft, with exit effectively unavailable because overruling course would repudiate its recent landmarks. From the university seat it is foreclosed autonomy: a compliance regime with litigation exposure. From the subordinated-community seat it is a closed courthouse door: protection from hostility purchased with the loss of targeted remedy. From the disfavored-applicant seat it is equal treatment finally delivered, with concrete positional gains. From the movement-lawyer seat it is vindication and career payoff. Same structure, divergent experienced types — computed by the engine from power, exit, and directional position, not asserted here.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low d: minority citizens (protected class), disfavored applicants (gain recipients), movement lawyers (vindication collectors). Declared victims derive high d: universities (constrained institutional payers) and subordinated communities (trapped payers, sitting nearest the full-target end because no exit exists). The dual position of minority citizens — shielded by the rule yet foreclosed from remedial tools — is handled by splitting the seat rather than overriding: racial_minority_citizens carries the protective benefit (low derived d) while historically_subordinated_communities carries the remedial cost (high derived d, amplified by trapped exit and national scope). No directionality_overrides are authored: the declared roles, exits, and scopes give the derivation everything it needs, and speculative overrides would only blur seats the structural data already separates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-enforced racial caste — is dead in its original Jim Crow form, and the rule has been re-scoped from shielding a subordinated class to barring all classifications including benign ones. The reading itself denies obsolescence: it claims the mandate is permanent (race is never relevant to legitimate state action). Authored founding_problem_status is therefore 'contested' rather than 'dead', because the parties genuinely dispute whether structural subordination persists in forms the original mandate reaches. The R5 mismatch consumer should read status=contested x verdict=world_rearranges as the live signal, with zombie-flag risk concentrated in the protective_function_atrophy omega: if hostile-classification litigation has receded, the rule increasingly acts only against benign uses — function narrowing while enforcement intensity rises, the classic shape of a mandate drifting from its founding object without admitting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the equal_protection_clause kernel. What would adoption of a sibling reading change structurally?',
    'Doctrinal tracking across appointment cycles: if the Court adopts the remedial reading, the beneficiary/victim sets invert (race-conscious programs become mandated, colorblind challengers become the constrained seats) and epsilon is re-authored upward; if the diversity reading returns, a middle band of permitted classifications reopens.',
    'Classification of this constraint is reading-relative; a sibling victory converts this story''s payer seats into beneficiaries and moves the rule''s incidence from forbidding to mandating classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-relativity of the constraint''s structure within the equal protection kernel.').

omega_variable(
    entrenchment_vs_protection,
    'Does the rule''s coordination function still dominate its incidence — protecting vulnerable minorities from hostile majorities — or has it come to operate mainly as entrenchment, disabling remedial tools while background disparities persist?',
    'Comparative outcome analysis across jurisdictions and periods: minority-protection outcomes under colorblind versus race-conscious regimes (pre/post Proposition 209 in California, pre/post SFFA admissions cohorts), plus litigation-mix data on hostile versus benign classifications challenged.',
    'If protection dominates, the constraint sits nearer pure coordination and the low authored epsilon is stable; if entrenchment dominates, the extraction component rises and the tangled_rope claim hardens toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_vs_protection, empirical, 'Whether the rule''s real work is minority protection or remedial foreclosure.').

omega_variable(
    formal_incidence_accounting,
    'Does the low authored extractiveness survive an accounting that includes foregone-remedy costs — the value of the race-conscious instruments the rule removes — or is it an artifact of measuring only the rule''s formal operation?',
    'Welfare-economic estimation of the opportunity cost imposed on remedy-seeking populations, set against the coordination value of the bright line (administrative savings, reduced racial factionalism, protection from hostile classification).',
    'Including foregone-remedy costs would raise epsilon materially and strengthen the asymmetric-incidence half of the tangled_rope claim; excluding them keeps epsilon near the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_incidence_accounting, conceptual, 'Whether epsilon should price the rule''s opportunity-cost incidence or only its formal application.').

omega_variable(
    judicial_identity_lock_persistence,
    'Will the judiciary''s institutional fusion with the formal-symmetry principle survive membership turnover, or is the lock contingent on the current appointment pipeline?',
    'Observe doctrine across successive appointment generations: whether newly seated justices treat the colorblind line as settled identity or as revisable precedent.',
    'An identity break would reopen remedial and diversity space rapidly through overruling cascades, converting this story''s structural constants into variables.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_identity_lock_persistence, empirical, 'Durability of the enforcing institution''s identity fusion with the reading.').

omega_variable(
    protective_function_atrophy,
    'Has the rule''s protective function — shielding minorities from hostile state action — atrophied as overtly hostile classifications receded, leaving enforcement aimed predominantly at benign and remedial uses?',
    'Litigation-mix time series: share of successful challenges directed at hostile versus benign classifications across the interval.',
    'If the protective share has collapsed, the rule''s residual function narrows toward foreclosure of remedies alone — a function-atrophy signature the lifecycle tracker should weight, even while enforcement intensity remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_function_atrophy, empirical, 'Whether the rule still performs its original protective work or mainly bars remedial uses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1978, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_colorblind_reading_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t1978, observed).
narrative_ontology:measurement(ep_colorblind_reading_tr_t1989, equal_protection_clause__colorblind_reading, theater_ratio, 1989, 0.1).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t1989, observed).
narrative_ontology:measurement(ep_colorblind_reading_tr_t1995, equal_protection_clause__colorblind_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t1995, observed).
narrative_ontology:measurement(ep_colorblind_reading_tr_t2003, equal_protection_clause__colorblind_reading, theater_ratio, 2003, 0.13).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t2003, observed).
narrative_ontology:measurement(ep_colorblind_reading_tr_t2007, equal_protection_clause__colorblind_reading, theater_ratio, 2007, 0.15).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t2007, observed).
narrative_ontology:measurement(ep_colorblind_reading_tr_t2014, equal_protection_clause__colorblind_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t2014, observed).
narrative_ontology:measurement(ep_colorblind_reading_tr_t2023, equal_protection_clause__colorblind_reading, theater_ratio, 2023, 0.18).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t2023, observed).
narrative_ontology:measurement(ep_colorblind_reading_tr_t2025, equal_protection_clause__colorblind_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(ep_colorblind_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ep_colorblind_reading_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.05).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t1978, observed).
narrative_ontology:measurement(ep_colorblind_reading_be_t1989, equal_protection_clause__colorblind_reading, base_extractiveness, 1989, 0.07).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t1989, observed).
narrative_ontology:measurement(ep_colorblind_reading_be_t1995, equal_protection_clause__colorblind_reading, base_extractiveness, 1995, 0.09).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t1995, observed).
narrative_ontology:measurement(ep_colorblind_reading_be_t2003, equal_protection_clause__colorblind_reading, base_extractiveness, 2003, 0.1).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t2003, observed).
narrative_ontology:measurement(ep_colorblind_reading_be_t2007, equal_protection_clause__colorblind_reading, base_extractiveness, 2007, 0.12).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t2007, observed).
narrative_ontology:measurement(ep_colorblind_reading_be_t2014, equal_protection_clause__colorblind_reading, base_extractiveness, 2014, 0.12).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t2014, observed).
narrative_ontology:measurement(ep_colorblind_reading_be_t2023, equal_protection_clause__colorblind_reading, base_extractiveness, 2023, 0.14).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t2023, observed).
narrative_ontology:measurement(ep_colorblind_reading_be_t2025, equal_protection_clause__colorblind_reading, base_extractiveness, 2025, 0.14).
narrative_ontology:measurement_basis(ep_colorblind_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ep_colorblind_reading_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t1978, observed).
narrative_ontology:measurement(ep_colorblind_reading_su_t1989, equal_protection_clause__colorblind_reading, suppression_requirement, 1989, 0.48).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t1989, observed).
narrative_ontology:measurement(ep_colorblind_reading_su_t1995, equal_protection_clause__colorblind_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t1995, observed).
narrative_ontology:measurement(ep_colorblind_reading_su_t2003, equal_protection_clause__colorblind_reading, suppression_requirement, 2003, 0.63).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t2003, observed).
narrative_ontology:measurement(ep_colorblind_reading_su_t2007, equal_protection_clause__colorblind_reading, suppression_requirement, 2007, 0.7).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t2007, observed).
narrative_ontology:measurement(ep_colorblind_reading_su_t2014, equal_protection_clause__colorblind_reading, suppression_requirement, 2014, 0.73).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t2014, observed).
narrative_ontology:measurement(ep_colorblind_reading_su_t2023, equal_protection_clause__colorblind_reading, suppression_requirement, 2023, 0.8).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t2023, observed).
narrative_ontology:measurement(ep_colorblind_reading_su_t2025, equal_protection_clause__colorblind_reading, suppression_requirement, 2025, 0.82).
narrative_ontology:measurement_basis(ep_colorblind_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% One clause, three constraints: the equal protection kernel decomposes into colorblind (this file), diversity, and remedial readings with different epsilon, different beneficiary/victim sets, and different permanence claims. The colorblind reading is currently upstream in doctrine: its victories (Croson, Adarand, Parents Involved, SFFA) shrink the operating space of the sibling readings, which survive in dissents, scholarship, and state politics. Cross-file links complete the constraint family; each sibling story should link back here and document its own epsilon over the same fixed referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
