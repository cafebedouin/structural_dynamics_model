% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations Regime — Punitive Liability Reading (Article 231 War Guilt Grounding)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the punitive-liability reading of the Versailles
 *   reparations kernel: the position, dominant among Allied officials and the
 *   drafters of Article 231, that Germany bore sole moral and financial
 *   responsibility for the total costs of the war and that reparations claims
 *   could legitimately extend to the full scope of that liability rather than
 *   being bounded by assessed, verifiable damage or German fiscal capacity.
 *   Under this reading, the war-guilt clause is not legal formality but
 *   substantive moral judgment, and the reparations schedule that follows
 *   from it is treated as a rightful — if severe — consequence of German
 *   conduct, enforceable through sanctions (the 1923 Ruhr occupation
 *   exemplifies peak enforcement) rather than negotiated settlement. The
 *   measured extraction peaks in 1923 at the height of enforcement
 *   (occupation, hyperinflation) and declines as successive revisions (Dawes
 *   1924, Young 1929, Lausanne 1932) progressively abandoned the unbounded
 *   punitive premise in practice, even though this reading's own internal
 *   logic never formally repudiated it.
 *
 * KEY AGENTS:
 *   - allied_creditor_states: primary beneficiary (institutional/arbitrage) — collect reparations, control commission terms
 *   - reparations_commission_officials: administers claims machinery under the punitive-liability premise
 *   - german_industrial_workers: primary target (powerless/trapped) — bear the transmission burden through inflation and in-kind extraction
 *   - german_taxpayers: bear fiscal burden with no exit or contest channel
 *   - weimar_fiscal_administration: administers payment under subordinated sovereignty, cannot dispute the guilt premise without repudiating the treaty
 *   - german_nationalist_political_factions: excluded objectors whose dissent registers only as domestic instability
 *   - international_economists: analytical observers assessing capacity-to-pay independent of both positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.81).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.72).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations Regime — Punitive Liability Reading (Article 231 War Guilt Grounding)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '08ef0186-6bdb-4dc8-9292-9a4b078277e3').
narrative_ontology:cs_kernel_codification('08ef0186-6bdb-4dc8-9292-9a4b078277e3', fixed_text).
narrative_ontology:cs_authority_grounding('08ef0186-6bdb-4dc8-9292-9a4b078277e3', extraction).
narrative_ontology:cs_interpretation_layer_present('08ef0186-6bdb-4dc8-9292-9a4b078277e3').
narrative_ontology:cs_reading_relation('08ef0186-6bdb-4dc8-9292-9a4b078277e3', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_reading_relation('08ef0186-6bdb-4dc8-9292-9a4b078277e3', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('08ef0186-6bdb-4dc8-9292-9a4b078277e3', foundational, sole_belligerent_moral_responsibility).
narrative_ontology:cs_axiom_status(sole_belligerent_moral_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('08ef0186-6bdb-4dc8-9292-9a4b078277e3', sole_belligerent_moral_responsibility, deontological).
narrative_ontology:cs_axiom('08ef0186-6bdb-4dc8-9292-9a4b078277e3', foundational, liability_unbounded_by_payer_capacity).
narrative_ontology:cs_axiom_status(liability_unbounded_by_payer_capacity, overridden).
narrative_ontology:cs_axiom_grounding('08ef0186-6bdb-4dc8-9292-9a4b078277e3', liability_unbounded_by_payer_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('08ef0186-6bdb-4dc8-9292-9a4b078277e3', sole_belligerent_responsibility_framework).
narrative_ontology:cs_drift_state('08ef0186-6bdb-4dc8-9292-9a4b078277e3', post_dawes_young_revision_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('08ef0186-6bdb-4dc8-9292-9a4b078277e3', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_reconstruction_fund_administrators).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, reparations_commission_officials).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_fiscal_administration).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, sole_belligerent_responsibility_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, total_war_cost_liability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, Britain, Belgium, and others hold reparations claims denominated against total German capacity rather than assessed damage, justified by Article 231's assignment of sole responsibility for the war. They set commission terms, threaten sanctions (Ruhr occupation) for default, and can revise schedules unilaterally through the Reparations Commission they control.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter).

% Administer assessment, collection, and enforcement of the payment schedule on behalf of the Allied powers, treating the war-guilt clause as settled legal ground for open-ended claims rather than a capped damages assessment. They control the machinery that converts the punitive-liability premise into enforceable fiscal obligation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, reparations_commission_officials, agenda_setter,
    institutional, generational, analytical, continental).

% Bear the transmission of reparations burden through currency instability, in-kind extraction of coal and industrial output, and the wage and employment effects of hyperinflation triggered partly by reparations-driven monetary policy. Have no channel to contest the liability assignment and no exit from the national economy absorbing it.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers, payer,
    powerless, biographical, trapped, national).

% Fund reparations payments through taxation and public borrowing under a legal framework (Article 231) that treats the obligation as flowing from established sole guilt rather than negotiated, capacity-bounded settlement. Individual taxpayers cannot exit the fiscal jurisdiction or contest the underlying liability claim.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers, payer,
    powerless, biographical, trapped, national).

% The German state apparatus must administer collection and transfer of reparations while its sovereign fiscal and monetary policy is subordinated to external creditor oversight (Dawes and Young plan supervision). It negotiates at the margins but cannot dispute the war-guilt premise the punitive reading grounds claims in without repudiating the treaty itself.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_fiscal_administration, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, weimar_fiscal_administration, excluded).

% Reject the sole-guilt premise entirely and argue for repudiation, but are excluded from the treaty-drafting and commission-administration process; their objection registers as domestic political instability rather than as input into the reparations framework itself.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_nationalist_political_factions, excluded,
    organized, generational, constrained, national).

% Analysts such as Keynes assessed the transfer problem and capacity-to-pay economics from outside both the Allied and German positions, producing independent evidence on whether the punitive-liability assessment was economically sustainable regardless of its moral premise.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a mechanism for compensating verified war damage to Allied civilian populations and infrastructure, coordinating collection and disbursement through a single commission rather than bilateral claims chaos.
% TRANSFER_FUNCTION: Moves resources — cash, coal, industrial equipment, shipping capacity — from the German state and by extension German taxpayers and workers to Allied creditor governments, justified by the sole-responsibility premise of Article 231 rather than by a capped damages assessment.
% ABSENT_VOICES: German nationalist and moderate political factions who dispute sole war guilt are excluded from the treaty's drafting and from the Reparations Commission's administration; their objection surfaces only as domestic unrest, not as a structural input into the claims framework.
% DISAPPEARANCE_RATIONALE: If the punitive-liability grounding of Article 231 were withdrawn overnight, the open-ended claims structure would collapse into a capacity-bounded settlement (as later actually occurred via the Dawes, Young, and Lausanne revisions), German fiscal sovereignty would be substantially restored, and Allied creditor states would lose the legal premise for extracting payment beyond assessed, bounded damage.
% FOUNDING_PROBLEM: Allied powers, having suffered severe wartime destruction, needed a legal and financial mechanism to compel compensation from the defeated power and to assign responsibility for the war's costs before financing postwar reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: Allied officials and reparations commission administrators attested the sole-guilt premise as settled fact justifying the schedule. Independent economists (Keynes, 'The Economic Consequences of the Peace') and, later, the Allied powers' own revision bodies (Dawes 1924, Young 1929, Lausanne 1932) attested from outside the original beneficiary coalition that the capacity-unbounded punitive reading was economically unsustainable and progressively abandoned in practice — corroboration external to the states collecting the payments.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at the story's representative point, peaking near 0.88 in 1923) because this reading treats the liability as effectively unbounded by capacity — the defining structural feature that distinguishes it from the limited_responsibility_reading. Suppression is authored high (0.72) because the punitive reading's persistence depended on active enforcement machinery (occupation, sanctions threat, commission oversight of German fiscal policy) rather than German consent or negotiated agreement. Theater ratio is moderate-low (0.28): the reconstruction-financing function was genuinely operative (war damage was real and compensable), but a growing share of enforcement activity over 1919-1925 defended the sole-guilt premise itself rather than verified damage claims, which is why theater rises through 1925 before declining as the revision process (Dawes/Young) reintroduced capacity constraints.
 *
 * PERSPECTIVAL GAP:
 *   From the Allied creditor seat, the punitive-liability reading experiences as legitimate compensation for verified aggression and destruction — a rope-like coordination solving the real problem of postwar reconstruction financing. From the German payer seats, the identical structure experiences as an enforced, capacity-unbounded extraction whose legal grounding (sole guilt) they never accepted and cannot contest. This is exactly the seat divergence the tangled_rope classification is meant to capture: a genuine coordination function (financing verified reconstruction) coexists with asymmetric extraction (liability decoupled from capacity, enforced through sanctions) running through the same treaty structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states and the commission officials administering their claims sit at the beneficiary end of directionality — the punitive premise is the legal instrument that authorizes their extraction and they control its enforcement. German workers and taxpayers sit at the full-target end: they are structurally trapped (no exit from German fiscal jurisdiction), bear the transmission mechanism (inflation, in-kind extraction) directly, and have no voice in the liability assignment itself. The weimar fiscal administration occupies an intermediate position — it is organized and negotiates at the margins (hence 'constrained' rather than 'trapped' exit) but cannot contest the underlying war-guilt premise without effectively repudiating the treaty, which is a different reading entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — compensating verified Allied war damage — was itself real and partially addressed by the reparations mechanism (mandatrophy is not declared for the coordination component). But the punitive-liability reading's specific contribution — grounding claims in total war-cost liability via Article 231 rather than assessed damage — increasingly outlived any defensible function as German capacity to pay was empirically established as insufficient (Keynes 1919, and later the Allied powers' own Dawes/Young revisions). The declining suppression_requirement trajectory (0.85 in 1923 to 0.40 by 1932) traces the gradual, informal abandonment of the punitive premise in practice even without formal renunciation — the classic pattern of a constraint's operative function eroding while its legal/moral framing persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_guilt_moral_vs_legal_status,
    'Is Article 231''s assignment of responsibility a substantive moral judgment (as the punitive reading holds) or a legal formality necessary to ground any reparations claim regardless of proportional fault (as the limited_responsibility_reading holds)?',
    'Historical-legal analysis of treaty drafting records and contemporaneous diplomatic correspondence distinguishing the clause''s intended legal function from its subsequent political deployment.',
    'If Article 231 is read as legal formality only, the punitive-liability reading''s grounding for unbounded claims dissolves and the constraint collapses toward the limited_responsibility_reading''s structure; if read as substantive moral judgment, the punitive reading''s high-ε structure is internally consistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_guilt_moral_vs_legal_status, conceptual, 'Whether Article 231 grounds moral or merely legal liability — the central interpretive fork between this reading and its limited_responsibility sibling.').

omega_variable(
    capacity_boundedness_of_punitive_claims,
    'Can a liability claim coherently remain ''total'' and capacity-unbounded once independent economic analysis (Keynes 1919) establishes that German fiscal capacity cannot sustain the schedule?',
    'Comparison of the original 1921 London Schedule assessment against the actual payment revisions (Dawes 1924, Young 1929) and the eventual near-total suspension (Lausanne 1932, Hoover Moratorium) to determine whether the punitive premise was ever operative in practice or immediately subject to informal capacity-bounding.',
    'If the punitive premise was never actually enforced at its stated scope, the punitive_liability_reading describes a nominal/legal claim rather than the operative extraction mechanism — suggesting the effective constraint German payers experienced was closer to a capacity-bounded arrangement despite the punitive legal framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_boundedness_of_punitive_claims, empirical, 'Whether the unbounded punitive claim was ever operative or was informally capacity-bounded from the outset.').

omega_variable(
    coercion_vs_consent_in_treaty_signing,
    'Does German formal acceptance of the treaty (under threat of renewed invasion) constitute legitimate consent sufficient to ground ongoing obligation under this reading, or does it constitute duress sufficient to void the obligation (as the repudiation_reading holds)?',
    'Comparative analysis against international-law standards for duress in treaty formation, applied retrospectively to the 1919 Versailles negotiating conditions.',
    'If duress voids consent, this reading''s claim to a binding punitive liability has no legitimate foundation regardless of its economic content — collapsing this reading toward repudiation on legal grounds rather than economic ones.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_vs_consent_in_treaty_signing, preference, 'Whether treaty acceptance under military threat constitutes valid consent — the fork between this reading and the repudiation_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.2).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.3).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1925, 0.35).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.32).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.28).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.62).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.74).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.88).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1925, 0.79).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.68).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.55).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.68).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.85).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.6).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__punitive_liability_reading, 0.1).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_hyperinflation_monetary_policy).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, dawes_plan_supervision_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the versailles_reparations_clauses kernel. The punitive_liability_reading authors high, capacity-unbounded ε grounded in Article 231's sole-guilt clause; the limited_responsibility_reading authors substantially lower ε bounded by economic viability; the repudiation_reading authors near-zero ε on grounds the entire obligation is illegitimate. All three describe the SAME treaty text but instantiate structurally distinct constraints because their beneficiary/victim structures and effective liability scope differ. Network edges also connect to downstream constraints this reading's enforcement helped produce (Weimar hyperinflation dynamics, the Dawes Plan supervisory regime that partially superseded it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
