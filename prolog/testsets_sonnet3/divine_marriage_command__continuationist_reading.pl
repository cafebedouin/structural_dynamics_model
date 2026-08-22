% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Continuationist Reading: Plural Marriage as Unrescinded Divine Command
 *   domain: religious_authority/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the continuationist reading of the
 *   divine_marriage_command kernel: the claim that the 1890 Manifesto
 *   suspending public solemnization of plural marriage was a prudential,
 *   duress-driven concession to federal prosecution, not a doctrinal
 *   rescission of the original 1843 revelation commanding plural marriage.
 *   Under this reading, plural marriage remains theologically binding and
 *   unbroken, fundamentalist splinter groups (FLDS and related communities)
 *   claim direct doctrinal continuity with the founding revelation, and
 *   federal and mainstream-LDS authority is treated as illegitimate external
 *   coercion rather than a source of internal correction. This is a distinct
 *   constraint from the substitutionist_reading (which holds monogamy is now
 *   doctrinally required by new revelation) and the
 *   coercion_visibility_reading (which treats the Manifesto as an
 *   acknowledged survival-driven capitulation whose legitimacy derives from
 *   institutional survival, not doctrinal continuity). The three readings
 *   share the same kernel — the historical Manifesto and the underlying 1843
 *   revelation — but instantiate structurally distinct constraints with
 *   different victim sets, different beneficiary structures, and different ε:
 *   the substitutionist reading has near-zero extraction once monogamy is
 *   normatively settled, while the coercion_visibility_reading's extraction
 *   is bounded by explicit acknowledgment of external constraint. This
 *   continuationist reading carries the highest sustained extraction because
 *   it authorizes ongoing plural marriage assignment inside closed
 *   communities with minimal external check.
 *
 * KEY AGENTS:
 *   - fundamentalist_priesthood_leadership: agenda_setter (institutional/arbitrage) — administers doctrine and marriage assignment, frames the Manifesto as duress not revision
 *   - senior_plural_husbands: beneficiary (powerful/constrained) — accumulate wives and status under the doctrine's continued authorization
 *   - plural_wives_in_closed_communities: payer (powerless/trapped) — bear reproductive and domestic labor with minimal placement autonomy
 *   - minor_brides: payer (powerless/trapped) — the starkest victim class, married below age of majority under the doctrine's continued claim
 *   - excommunicated_dissenters: payer (powerless/trapped) — bear the cost of exit in lost community, family, and economic support
 *   - federal_and_state_authorities: excluded (institutional/analytical) — enforce law from outside the community's own interpretive frame, treated as illegitimate coercion within it
 *   - mainstream_lds_church_leadership: excluded (institutional/analytical) — holds the rival substitutionist reading; treated by this reading as having capitulated rather than received legitimate new revelation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Continuationist Reading: Plural Marriage as Unrescinded Divine Command").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, 'c51a4db7-6647-4fd4-a171-8222949f7a9f').
narrative_ontology:cs_kernel_codification('c51a4db7-6647-4fd4-a171-8222949f7a9f', fixed_text).
narrative_ontology:cs_authority_grounding('c51a4db7-6647-4fd4-a171-8222949f7a9f', lineage).
narrative_ontology:cs_interpretation_layer_present('c51a4db7-6647-4fd4-a171-8222949f7a9f').
narrative_ontology:cs_reading_relation('c51a4db7-6647-4fd4-a171-8222949f7a9f', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('c51a4db7-6647-4fd4-a171-8222949f7a9f', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('c51a4db7-6647-4fd4-a171-8222949f7a9f', foundational, plural_marriage_command_never_rescinded).
narrative_ontology:cs_axiom_status(plural_marriage_command_never_rescinded, holdable).
narrative_ontology:cs_axiom_grounding('c51a4db7-6647-4fd4-a171-8222949f7a9f', plural_marriage_command_never_rescinded, theological).
narrative_ontology:cs_axiom('c51a4db7-6647-4fd4-a171-8222949f7a9f', foundational, external_prosecution_cannot_alter_internal_doctrine).
narrative_ontology:cs_axiom_status(external_prosecution_cannot_alter_internal_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c51a4db7-6647-4fd4-a171-8222949f7a9f', external_prosecution_cannot_alter_internal_doctrine, deontological).
narrative_ontology:cs_reference_frame('c51a4db7-6647-4fd4-a171-8222949f7a9f', unbroken_1843_revelation_priesthood_continuity).
narrative_ontology:cs_drift_state('c51a4db7-6647-4fd4-a171-8222949f7a9f', post_1890_manifesto_and_subsequent_federal_prosecution, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('c51a4db7-6647-4fd4-a171-8222949f7a9f', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_priesthood_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, senior_plural_husbands).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_wives_in_closed_communities).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, minor_brides).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, excommunicated_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers priesthood authority in splinter communities (FLDS and kindred groups), determines who may enter, be reassigned, or be expelled from plural marriages, and adjudicates which revelations remain binding. Frames the 1890 Manifesto as a temporary concession extracted under federal duress rather than a rescission of the original command, and uses this framing to justify continued practice and to discipline dissent. Holds effective control over marriage assignment, property, and community membership, and can relocate or reconstitute the community if outside pressure intensifies.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_priesthood_leadership, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, fundamentalist_priesthood_leadership, beneficiary).

% Accumulate multiple wives and the domestic, reproductive, and social capital that accompanies plural households, legitimated by the continuationist reading of the kernel. Their standing within the community depends on continued institutional endorsement of the doctrine; they benefit from the arrangement's persistence but are not the ones administering it.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, senior_plural_husbands, beneficiary,
    powerful, generational, constrained, regional).

% Enter marriages arranged or approved by priesthood leadership, often with little say in placement or sequence of wives. Economic dependency, social isolation from outside networks, and the doctrine's framing of obedience as salvific make leaving costly — loss of children, community, and often literacy in navigating the outside world. Reality-testing contacts outside the community are frequently restricted.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_wives_in_closed_communities, payer,
    powerless, biographical, trapped, local).

% In some documented cases, girls below the age of majority are married to older men under the doctrine's continued authority claim. They have no independent economic standing, no external legal recourse recognized within the community, and are the starkest evidence of the asymmetric cost the continuationist reading imposes.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, minor_brides, payer,
    powerless, biographical, trapped, local).

% Members (frequently 'lost boys' and women who question placement or doctrine) who are expelled or flee, losing family contact, economic support, and community identity built over a lifetime. Their departure is treated internally as proof of the doctrine's continued binding force rather than as evidence against it.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, excommunicated_dissenters, payer,
    powerless, biographical, trapped, local).

% Enforce anti-bigamy statutes and investigate abuse allegations from outside the community's own interpretive frame. Their legal authority is precisely what the continuationist reading characterizes as illegitimate external coercion rather than a source of doctrinal correction, which is why their voice carries no weight inside the priesthood's own adjudication.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_and_state_authorities, excluded,
    institutional, generational, analytical, national).

% Holds the substitutionist reading and formally excommunicates practitioners of plural marriage. From the continuationist community's perspective, mainstream leadership is not a neutral observer but the very body that capitulated to federal pressure; its authority to declare the doctrine rescinded is exactly what this reading denies.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_lds_church_leadership, excluded,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, fundamentalist_priesthood_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internally coherent basis for organizing marriage, inheritance, and priesthood succession within a community that regards itself as the sole continuous heir to an unrescinded 19th-century revelation, allowing coordinated resistance to assimilation pressure.
% TRANSFER_FUNCTION: Moves reproductive labor, domestic labor, and deference from plural wives (and disproportionately from the youngest and most recently placed wives) to senior men and to the priesthood leadership that arranges placements and controls community resources.
% ABSENT_VOICES: Plural wives who would prefer monogamous or self-chosen unions, minors who cannot consent, and departed members are excluded from the community's own doctrinal adjudication; their objections are recorded, if at all, only in state investigations and survivor testimony outside the tradition.
% DISAPPEARANCE_RATIONALE: If the continuationist reading were abandoned overnight in favor of the substitutionist reading actually taking hold, marriage assignments would need renegotiation, priesthood succession claims tied to plural lineage would lose their doctrinal basis, and the community's central claim to unbroken continuity with the founding revelation would collapse, likely triggering fragmentation or dissolution of the splinter structure itself.
% FOUNDING_PROBLEM: The founding problem, as this reading states it, was to preserve a divine command (plural marriage as commanded by revelation) against a federal government using criminal prosecution and property seizure to force its abandonment — a problem of institutional survival under external coercion, not a problem of doctrinal error needing correction.
% FOUNDING_PROBLEM_CORROBORATION: Priesthood leadership and senior husbands attest the founding problem (external coercion against a valid revelation) remains live and unresolved. Historians of Mormonism, federal court findings in bigamy and child-welfare prosecutions, and survivor testimony from excommunicated members — all outside the benefiting parties — instead corroborate that the practical function has shifted from preserving revealed doctrine to consolidating patriarchal authority and resource control within closed communities; no source outside the continuationist leadership itself corroborates the founding-problem framing as currently operative.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is authored high because the reading authorizes an ongoing transfer of domestic and reproductive labor from plural wives to senior men and to priesthood leadership, with placement decisions made by leadership rather than by the women involved, and because the population most affected (minors, isolated wives) has the least capacity to contest the arrangement. Suppression (0.72) is authored higher than extraction because persistence depends on active enforcement — excommunication, relocation, isolation from outside informational networks — not merely on participant preference. Theater ratio (0.40) reflects that a substantial share of the doctrine's maintenance work is genuinely functional (priesthood succession, community cohesion) rather than purely performative, though the share devoted to defending the continuationist framing against internal and external challenge has grown over the interval. Accessibility collapse (0.58) and resistance (0.74) are authored moderate-high: alternatives (monogamous marriage, exit to mainstream society) exist and are known to exist, so collapse is not near-total, but active resistance from dissenters, state authorities, and survivor networks is substantial and growing.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood leadership's seat, the arrangement is genuine doctrinal continuity under threat, and the engine's classification of the payer seats as extractive would read, internally, as a failure to recognize legitimate religious authority. From the plural-wife and minor-bride seats, the same structural facts — assignment without consent, restricted exit, isolation from outside contact — compute as active extraction regardless of the doctrinal framing. This divergence is exactly what the per-seat computation is designed to surface: the claim (tangled_rope, since it retains a genuine coordination function — community cohesion and continuity claims — alongside clear asymmetric extraction) and the metrics are authored independently and are not reconciled to each other.
 *
 * DIRECTIONALITY LOGIC:
 *   Priesthood leadership sits nearest the beneficiary end: institutional power, arbitrage-grade exit (can relocate the community or reconstitute leadership structures under pressure), and direct control over the doctrine's application. Senior husbands are secondary beneficiaries with less arbitrage capacity than leadership but still constrained rather than trapped exit. Plural wives, minor brides, and excommunicated dissenters are all powerless with trapped exit, which the derivation chain correctly pushes toward the full-target end of directionality — economic dependency, restricted outside contact, and doctrinal framing of obedience as salvific all compound to make exit costly. No override is needed here; the beneficiary/victim declarations plus exit options already capture the intended asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing (preserving a divine command against federal coercion) may have been genuinely live in 1890 but the corroboration record — historians, federal court findings, survivor testimony — indicates the practical function has shifted toward consolidating patriarchal authority and resource control rather than preserving an embattled revelation. The mismatch between founding_problem_status ('contested', trending toward 'dead' outside the benefiting parties) and disappearance_verdict ('world_rearranges', since real institutional structures depend on the doctrine's continuity) is the mandatrophy signal: this is not simple coordination cover, nor is it pure inertia (leadership does capture concentrated benefit, ruling out piton), but a hybrid that must be actively enforced against both external law and internal dissent — which is the structural signature of tangled_rope rather than snare (a genuine, if contested, coordination function around community continuity persists alongside the asymmetric extraction) or piton (there IS a concentrated beneficiary — leadership and senior husbands — which a piton by definition lacks).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_as_revision_vs_suspension,
    'Was the 1890 Manifesto a genuine doctrinal rescission (substitutionist reading), an openly acknowledged survival concession (coercion_visibility reading), or a merely prudential suspension leaving the underlying command intact (this, continuationist, reading)? The three readings cannot all be correct about the same historical document''s binding force.',
    'There is no neutral resolution mechanism available in principle: the question turns on contested claims about the nature of continuing revelation and institutional authority that the traditions themselves adjudicate differently. Historical evidence (private correspondence, subsequent sermons, church court records) can bear on which reading better fits the documentary record, but cannot settle the theological question of whether a suspension under duress counts as rescission.',
    'If the continuationist reading is correct, fundamentalist splinter groups hold a legitimate, if suppressed, doctrinal position and the extraction identified here is the cost of defending a genuine (if contested) religious commitment against external coercion. If the substitutionist or coercion_visibility readings are correct, the continuationist reading is better characterized as a cover story maintaining patriarchal control after its doctrinal warrant lapsed — pushing this constraint closer to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manifesto_as_revision_vs_suspension, conceptual, 'Whether the Manifesto rescinded, suspended, or merely acknowledged coercion against the original plural marriage command — the central kernel contest.').

omega_variable(
    minor_bride_incidence_and_causal_link,
    'How directly does the continuationist doctrinal reading (as opposed to other features of closed-community structure, such as geographic isolation or charismatic authority independent of the doctrine) drive the incidence of marriage involving minors?',
    'Comparative analysis across fundamentalist splinter communities with varying doctrinal emphasis and varying degrees of isolation, correlated with documented incidence of underage marriage, would help separate the doctrine''s causal contribution from other community features.',
    'If the doctrine is the primary driver, the extractiveness and suppression scores authored here are conservative; if isolation and charismatic authority are doing most of the work independent of the specific continuationist claim, some of the extraction attributed to this constraint may actually belong to a separate constraint (community isolation mechanisms) that should be decomposed out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minor_bride_incidence_and_causal_link, empirical, 'Whether the continuationist doctrine itself, versus community isolation more broadly, drives the most severe victim outcomes.').

omega_variable(
    internal_vs_external_legitimacy_standard,
    'Should the legitimacy of the continuationist reading be assessed by its own internal doctrinal standards (in which case federal and mainstream-LDS objections are simply external noise) or by an external standard that treats state law and consent norms as legitimate correctives regardless of the community''s own framing?',
    'This is not empirically resolvable; it is a framing choice about which authority has standing to adjudicate a religious community''s internal doctrine. The choice determines whether excluded voices (federal authorities, mainstream church leadership) count as corroboration or as illegitimate interference.',
    'Adopting an internal-standard-only frame would lower the effective suppression and extractiveness scores (external law is simply noise to be resisted, not evidence of harm); adopting an external-standard frame, as this story does by treating federal findings and survivor testimony as corroborating sources, raises them. This story deliberately adopts the external-standard frame for the founding_problem_corroboration field while still authoring the coordination_function honestly from the community''s own internal perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_external_legitimacy_standard, preference, 'Whether external legal and ex-member testimony counts as legitimate corroboration or as illegitimate outside interference, per the reading''s own framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement_basis(divi_tr_t1890, observed).
narrative_ontology:measurement(divi_tr_t1935, divine_marriage_command__continuationist_reading, theater_ratio, 1935, 0.25).
narrative_ontology:measurement_basis(divi_tr_t1935, observed).
narrative_ontology:measurement(divi_tr_t1970, divine_marriage_command__continuationist_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement_basis(divi_tr_t1970, observed).
narrative_ontology:measurement(divi_tr_t1998, divine_marriage_command__continuationist_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement_basis(divi_tr_t1998, observed).
narrative_ontology:measurement(divi_tr_t2008, divine_marriage_command__continuationist_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(divi_tr_t2008, observed).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(divi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.45).
narrative_ontology:measurement_basis(divi_be_t1890, observed).
narrative_ontology:measurement(divi_be_t1935, divine_marriage_command__continuationist_reading, base_extractiveness, 1935, 0.52).
narrative_ontology:measurement_basis(divi_be_t1935, observed).
narrative_ontology:measurement(divi_be_t1970, divine_marriage_command__continuationist_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement_basis(divi_be_t1970, observed).
narrative_ontology:measurement(divi_be_t1998, divine_marriage_command__continuationist_reading, base_extractiveness, 1998, 0.63).
narrative_ontology:measurement_basis(divi_be_t1998, observed).
narrative_ontology:measurement(divi_be_t2008, divine_marriage_command__continuationist_reading, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement_basis(divi_be_t2008, observed).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(divi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement_basis(divi_su_t1890, observed).
narrative_ontology:measurement(divi_su_t1935, divine_marriage_command__continuationist_reading, suppression_requirement, 1935, 0.58).
narrative_ontology:measurement_basis(divi_su_t1935, observed).
narrative_ontology:measurement(divi_su_t1970, divine_marriage_command__continuationist_reading, suppression_requirement, 1970, 0.63).
narrative_ontology:measurement_basis(divi_su_t1970, observed).
narrative_ontology:measurement(divi_su_t1998, divine_marriage_command__continuationist_reading, suppression_requirement, 1998, 0.68).
narrative_ontology:measurement_basis(divi_su_t1998, observed).
narrative_ontology:measurement(divi_su_t2008, divine_marriage_command__continuationist_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement_basis(divi_su_t2008, observed).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(divi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.08).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint, substitutionist_reading, and coercion_visibility_reading form a three-member family decomposing the colloquial label 'the LDS Manifesto and plural marriage.' Each instantiates a structurally distinct claim about the same historical kernel (the 1843 revelation and 1890 Manifesto) with different ε: this continuationist reading is the most extractive (0.68) because it authorizes ongoing plural marriage assignment with minimal external check; the substitutionist reading has near-zero ongoing extraction once monogamy is doctrinally settled as the binding rule; the coercion_visibility reading's extraction is intermediate and bounded by its explicit acknowledgment of external constraint as the source of legitimacy rather than doctrinal continuity or supersession. All three share the same kernel_id (divine_marriage_command) and must be read together to understand the full contest; none is complete alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
