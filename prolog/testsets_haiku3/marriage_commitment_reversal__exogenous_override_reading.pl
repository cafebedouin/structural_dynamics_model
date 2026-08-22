% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercion of Polygamy Abandonment (Exogenous Override Reading)
 *   domain: religious_institutional/political_theology/commitment_systems
 *
 * SUMMARY:
 *   In 1882, the federal government threatened the LDS church with statehood
 *   denial, property seizure, and criminal prosecution of leadership unless
 *   it abandoned the practice of polygamous marriage ('plural marriage'),
 *   which church doctrine (Section 132 of the Doctrine and Covenants) defined
 *   as a celestial covenant necessary for exaltation. The church faced a
 *   choice: preserve doctrine and practice, face institutional destruction;
 *   or abandon practice while retaining doctrine. The institutional
 *   leadership chose a third path — a public cessation of practice without
 *   internal doctrinal revision. Section 132 was never renounced,
 *   reinterpreted, or superseded by new revelation in this reading's frame.
 *   Instead, doctrine persists in a celestial/afterlife register while
 *   practice is suspended in the temporal world. This reading instantiates
 *   that outcome as an exogenous override: the constraint is federal coercion
 *   extracting institutional compliance without granting internal legitimacy
 *   to the reversal. The constraint persists because federal force maintains
 *   it; the institutional gap (doctrine preserved, practice suspended)
 *   persists because no internal doctrinal work was done to resolve it. This
 *   is distinct from the endogenous_reinterpretation_reading, where divine
 *   revelation reframes God's will under changed circumstances, and from the
 *   practice_doctrine_gap reading, which treats the gap itself as the
 *   structural fact.
 *
 * KEY AGENTS:
 *   - LDS institutional leadership: administers coerced practice reversal while preserving doctrine; bears the cost of structural hypocrisy
 *   - Polygamist saints and plural wives: identity-locked faithful forced to choose between doctrinal violation (abandoning practice) and institutional exclusion (continuing practice)
 *   - Federal territorial authority & legislative body: the coercive seat, benefiting from institutional capitulation and territorial control; maintains the extractive threat
 *   - Mainstream American political culture: receives vindication of monogamous normativity; implicitly benefits from the church's subordination to civil law
 *   - Section 132 itself (analyzed as non-actor entity): preserved doctrine that is never renounced; its preservation is the evidence of the external-force frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.81).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercion of Polygamy Abandonment (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '6700c7a0-37fd-4aa8-9fb8-a5c696b4560e').
narrative_ontology:cs_kernel_codification('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', fixed_text).
narrative_ontology:cs_authority_grounding('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', extraction).
narrative_ontology:cs_interpretation_layer_present('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e').
narrative_ontology:cs_reading_relation('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', marriage_commitment_reversal__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', foundational, practice_reversal_externally_coerced).
narrative_ontology:cs_axiom_status(practice_reversal_externally_coerced, holdable).
narrative_ontology:cs_axiom_grounding('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', practice_reversal_externally_coerced, empirically_contingent).
narrative_ontology:cs_axiom('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', secondary, doctrine_preservation_signals_inauthentic_reversal).
narrative_ontology:cs_axiom_status(doctrine_preservation_signals_inauthentic_reversal, holdable).
narrative_ontology:cs_axiom_grounding('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', doctrine_preservation_signals_inauthentic_reversal, instrumental).
narrative_ontology:cs_reference_frame('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', celestial_marriage_doctrine_as_eternal_principle).
narrative_ontology:cs_drift_state('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', post_edmunds_act_coercion_1882_1890, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6700c7a0-37fd-4aa8-9fb8-a5c696b4560e', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_authority).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, mainstream_american_political_culture).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, polygamist_saints_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preserves the doctrinal principle of celestial marriage (Section 132) in theological canon while publicly abandoning polygamous practice in compliance with federal threat. Administers a dual-doctrine structure: the principle remains binding doctrine for the afterlife but practice is suspended in the temporal world. Bears the cost of doctrinal incoherence and institutional identity bifurcation — the marriage covenant that defines salvation doctrine cannot be lived by the faithful.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership, agenda_setter,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership, payer).

% Hold the lived polygamous commitment as doctrinally binding and spiritually necessary for salvation. Forced to choose between continuing the practice (excommunication or legal prosecution) and abandoning it (doctrinal violation of Section 132 covenant). Their identity is fused with the practice — leaving the community or breaking the commitment dissolves both their spiritual standing and their social world. Many chose prosecution or exile; those who stayed live in doctrinal violation of what they believe saves them.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, polygamist_saints_faithful, payer,
    powerless, biographical, identity_locked, national).

% Extracts institutional capitulation from the LDS church through threat of statehood denial, property seizure, and criminal prosecution of leadership and faithful. Gains territorial control, legal uniformity (monogamous marriage as sole recognized form), and institutional subordination of the church to civil authority. Can exit the confrontation at any moment by withdrawing threats; the church cannot exit without renouncing either practice or doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_authority, beneficiary,
    institutional, generational, arbitrage, national).

% Enacts and enforces the Edmunds Act (1882) and Edmunds-Tucker Act (1887) criminalizing polygamy and threatening the church's existence if it does not abandon the practice. Sets the terms of institutional compliance. Justifies coercion as enforcement of national legal uniformity and civilization standards. Controls the federal enforcement machinery and can modulate the threat.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_legislative_body, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives the impression of institutional reform and national legal coherence — the church has 'abandoned' polygamy and entered the American mainstream. Polygamy remains culturally marked as deviance; the church's public capitulation confirms national monogamous normativity. Supports the federal coercion as legitimate civilization-building and moral progress.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, mainstream_american_political_culture, beneficiary,
    organized, generational, mobile, national).

% Section 132 (the Doctrine and Covenants passage canonizing celestial marriage and plural union) remains preserved, unchanged, in the official scriptural canon. It is never renounced, reinterpreted, or superseded by internal divine revelation. This non-action is itself the signal: doctrine persists while practice is suspended by external force, creating an irreducible gap between stated principle and lived reality.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_doctrine, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_doctrine).

% Are implicitly vindicated by the federal coercion of the LDS church — their monogamous orthodoxy is validated as the only legitimate religious marriage form. They do not need to argue against polygamy; the federal government enforces the argument. Would object to alternative religious marriage forms if they were permitted, but are not in the conversation when the terms of LDS compliance are negotiated.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, competing_religious_denominations, excluded,
    organized, generational, constrained, national).

% Cannot achieve statehood while the LDS church practices polygamy — federal law explicitly conditions admission on the church's abandonment of the practice. They have structural incentive to pressure the church but no direct negotiating power; the federal government holds the coercive power and uses it on their behalf.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, territorial_statehood_applicants, excluded,
    institutional, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_authority).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal-territorial enforcement apparatus coordinates national legal uniformity around monogamous marriage as the sole recognized civil form and resolves the conflict between territorial law and theocratic practice by asserting federal supremacy.
% TRANSFER_FUNCTION: Transfers institutional autonomy from the LDS church to the federal government: the church surrenders control over its core religious practice (marriage covenant) in exchange for institutional survival. The doctrine is transferred into a supernatural/afterlife register (celestial marriage preserved as eternal principle, practice abandoned in temporal world), which is a form of rhetorical neutralization rather than genuine retention.
% ABSENT_VOICES: Polygamist saints forced into exogamy, ejection, or doctrinal violation are structurally excluded from the negotiation between federal authority and institutional leadership. Their lived commitment is the sacrifice; their voice is not heard in the settlement. Women plural wives especially are excluded — their spousal status, economic security, and doctrinal standing are erased by the institutional capitulation, but they are not parties to the negotiation.
% DISAPPEARANCE_RATIONALE: If federal coercion ceased, the LDS church would immediately face a choice: reinstitute polygamist practice under the preserved doctrine, or sustain the practice suspension without doctrinal authority (continued hypocrisy). Either path rearranges the institutional and legal landscape — statehood applications would face renewed obstacles, federal law would lose its enforcement vector for national marriage uniformity, and the religious-secular boundary would shift. The constraint's disappearance would unmask the doctrine-practice gap that coercion currently keeps hidden.
% FOUNDING_PROBLEM: Federal determination to enforce territorial legal uniformity and incorporate Utah Territory into the United States required elimination of theocratic plural marriage as incompatible with national governance and 'civilization standards.'
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative record (Edmunds Act debates, Edmunds-Tucker framing) explicitly states the founding problem: territorial control and legal uniformity are preconditions for statehood. Contemporary federal officials and politicians attest the problem. The LDS church leadership attests statehood admission required abandonment of practice. Independent historical analysis from outside the benefiting parties confirms federal intent to use coercion to enforce institutional capitulation, not to debate doctrine on its merits.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 by 1896) because the federal government extracts a fundamental institutional prerogative — the church surrenders control over its definition of the marriage covenant and the salvation contract that covenant implies. The institutional leadership retains formal administrative power but under duress; their choices are constrained by the federal threat to the point where no genuine discretion remains. Suppression is high (0.79 by 1896) because the coercion works through criminal law, property seizure threat, and statehood denial — structural mechanisms that leave no exit for the institutional leadership short of capitulation or institutional destruction. Theater is elevated (0.62 by 1896) because the 'solution' created by the coercion is performative: practice is publicly abandoned but doctrine is privately retained, creating an institutional show of compliance that masks the underlying doctrine-practice gap. The measurement series track the intensification of federal coercion (Edmunds Act 1882, Edmunds-Tucker Act 1887, property seizure proceedings 1887-1890) and then plateau at the point of institutional capitulation (1890 Manifesto, statehood admission 1896). The values remain stable from 1896-1907 because the constraint has achieved its extractive goal; further intensification is unnecessary as the institutional compliance is secured.
 *
 * PERSPECTIVAL GAP:
 *   Federal territorial authority and LDS institutional leadership compute radically differently. From the federal seat, the constraint is successful coordination enforcing national legal uniformity and civilizing a theocratic outlier — a genuine coordination problem solved, if at high cost to the church. From the institutional leadership seat, the constraint is an extortionate takeover of institutional sovereignty disguised as legal reform — the leadership administers compliance under duress, not from conviction. From the polygamist saints' seat (identity-locked victims), the constraint is not a coordination problem at all; it is a betrayal by institutional leadership of the doctrine that binds them, imposed by external force they never agreed to negotiate with. The engine computes these divergences from the structural data: the federal seat has arbitrage exit options and high power, the institutional leadership seat is trapped with no genuine discretion, and the saints' seat is identity-locked with exit that destroys identity. These structural facts drive the per-seat type computations — what the federal seat experiences as successful coordination, the trapped victim seat experiences as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from who benefits and who bears costs under coercion. Federal authority benefits (gains territorial control, enforces legal uniformity, claims a victory for civilization) — directionality near beneficiary end (d ≈ 0.1). LDS institutional leadership nominally administers but is trapped (retains no genuine discretion once the federal threat is credible) — directionality near target end despite the agenda_setter role (d ≈ 0.7, overridden from the derived value which would be lower because they administer). Polygamist saints are trapped and identity-locked (cannot exit without spiritual dissolution and community loss) — directionality at the full target end (d ≈ 1.0). The institutional leadership gets an override because the nominal role (agenda_setter) does not capture the structural reality: they set the agenda within the constraint of federal coercion, but the coercion has already determined the only acceptable outcome. An override lifts the directionality from where the nominal role would place it (lower, as if the leadership were genuine agenda-setters) to where the coercion places it (higher, as if they were targets administering the constraint against their own interests).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint classifies as tangled_rope rather than snare because it retains a coordination function: the federal enforcement does solve a genuine coordination problem (territorial legal uniformity) and does produce a coordination benefit (statehood is achieved; the church gains institutional survival). The problem is not a pure snare because both seats get something — the federal seat gets law uniformity and territorial control, the church seat gets institutional survival (at the cost of institutional bifurcation). The tangled_rope structure is held together by asymmetric extraction: the federal seat extracts institutional sovereignty while the church seat retains institutional form. If the constraint were a pure snare, the federal government would destroy the church rather than extract compliance; instead, it leaves the institutional structure intact but subordinated to federal law. The constraint would become a snare if the church were given no exit except destruction; instead, it is given the exit of practice abandonment, which the institutional leadership takes. The extraction is the cost of that exit — you can survive, but only by surrendering this core prerogative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    woodruff_vision_authenticity,
    'Was the 1890 Manifesto''s reported divine vision an authentic theological reinterpretation, or a rationalization for externally imposed compliance with federal threat?',
    'Historical analysis of Woodruff''s private diaries, institutional decision-making records, and contemporaneous communications from federal officials. Comparison with other cases where institutions claim divine authorization for practical capitulation.',
    'If the vision was authentic (or if authentic divine reinterpretation is indistinguishable from rational institutional response to coercion), this reading collapses into the endogenous_reinterpretation_reading. If the vision was a rationalization layer applied after the decision was already forced by federal coercion, this reading holds as the structural account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(woodruff_vision_authenticity, empirical, 'Whether the institutional reversal was internally legitimated or externally coerced.').

omega_variable(
    doctrine_preservation_intent,
    'Is the preservation of Section 132 in the scriptural canon a deliberate signal that doctrine persists while practice is suspended, or merely an artifact of institutional record-keeping with no intentional significance?',
    'Analysis of institutional statements and decisions regarding scriptural revision. If the church had made a clear theological decision to renounce celestial marriage doctrine, the canon would reflect that choice (as other doctrinal reversals are reflected through new scripture or explicit disavowal). The non-action (preserving the unchanged canon) is itself a choice, distinguishable from active revision.',
    'If preservation is intentional, the doctrine-practice gap is deliberate and sustained by institutional design, supporting this exogenous-override reading. If preservation is accidental or administratively inert, the distinction between this reading and endogenous_reinterpretation collapses — the readings become differently-framed accounts of the same institutional fact rather than structurally distinct claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_preservation_intent, conceptual, 'Whether doctrine preservation is a structural fact or an administrative artifact.').

omega_variable(
    suppression_internalization_trajectory,
    'Did the measured suppression persist as structural (federal legal threat) or become internalized (Saints came to believe practice abandonment is theologically correct, not just federally mandated)?',
    'Post-1896 generational analysis: track whether Saints who adopted monogamy under coercion later testified that the doctrine itself had changed (indicating internalization), or whether they retained belief in Section 132 while complying with practice suspension (indicating structural suppression persisting despite compliance). Generational drift in believer testimony would show internalization; stable doctrine + compliant practice would show structural suppression carrying forward.',
    'If suppression became internalized after ~1910-1920, the constraint''s effective suppression increases (targets carry it after coercion officially ends). If suppression remained structural (dependent on federal enforcement and institutional coercion), the constraint''s reach is limited by the enforcement apparatus — it ends when federal threat ends, not when belief changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Structural vs. internalized suppression mechanism in post-1896 generations.').

omega_variable(
    alternative_sibling_readings_kernel,
    'Which sibling reading best describes the actual institutional mechanism: endogenous divine reinterpretation, exogenous federal coercion, or structural doctrine-practice gap as the primary fact?',
    'Analysis of institutional intent, decision-making sequence, and the role of federal pressure in the timeline. The readings make falsifiable claims about causal structure that can be tested against historical evidence about federal pressure timing, institutional decision sequencing, and explicit statements of institutional intent.',
    'The classification of the constraint (tangled_rope in this reading) would shift to snare (in an extreme coercion-first reading) or to rope (in an endogenous_reinterpretation reading that emphasizes coordinating national legal uniformity as a legitimate shared problem). Choosing among the readings changes the per-seat type computations and the assessment of whether the reversal was extractive or coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_sibling_readings_kernel, conceptual, 'Framing under-determination across kernel readings of the marriage-commitment reversal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1882, 1907).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.41).
narrative_ontology:measurement_basis(marr_tr_t1882, observed).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.48).
narrative_ontology:measurement_basis(marr_tr_t1887, observed).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.58).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1896, 0.62).
narrative_ontology:measurement_basis(marr_tr_t1896, observed).
narrative_ontology:measurement(marr_tr_t1907, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1907, 0.62).
narrative_ontology:measurement_basis(marr_tr_t1907, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.68).
narrative_ontology:measurement_basis(marr_be_t1882, observed).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.76).
narrative_ontology:measurement_basis(marr_be_t1887, observed).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.79).
narrative_ontology:measurement_basis(marr_be_t1890, observed).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1896, 0.81).
narrative_ontology:measurement_basis(marr_be_t1896, observed).
narrative_ontology:measurement(marr_be_t1907, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1907, 0.81).
narrative_ontology:measurement_basis(marr_be_t1907, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.62).
narrative_ontology:measurement_basis(marr_su_t1882, observed).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.71).
narrative_ontology:measurement_basis(marr_su_t1887, observed).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.76).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1896, 0.79).
narrative_ontology:measurement_basis(marr_su_t1896, observed).
narrative_ontology:measurement(marr_su_t1907, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1907, 0.79).
narrative_ontology:measurement_basis(marr_su_t1907, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_reversal kernel. The exogenous_override_reading treats the practice reversal as imposed by federal coercion without internal doctrinal resolution. The endogenous_reinterpretation_reading treats it as theologically authentic reinterpretation via divine vision. The practice_doctrine_gap reading centers the gap itself (preserved doctrine, suspended practice) as the primary structural fact. All three are readings of the same kernel; they have different eps, different victim/beneficiary sets, and different empirical status. Link them via affects_constraints and distinguish via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
