% ============================================================================
% CONSTRAINT STORY: first_amendment_1951__ninth_schedule_immunity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_1951__ninth_schedule_immunity_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: first_amendment_1951__ninth_schedule_immunity_reading
 *   human_readable: Ninth Schedule Constitutional Immunity (First Amendment 1951 Reading)
 *   domain: legal/constitutional/india
 *
 * SUMMARY:
 *   The Ninth Schedule, introduced via the First Amendment in 1951 as part of
 *   Nehru's land reform package, became constitutional history's most
 *   consequential jurisdictional vault. Land reform statutes deposited into
 *   Schedule Nine were immunized from judicial review under Articles 14, 19,
 *   and 31 of the Constitution — they could not be challenged on grounds of
 *   violation of fundamental rights. The reading instantiated here treats the
 *   Ninth Schedule as the First Amendment's permanent invention: a mechanism
 *   for suppressing judicial review of deposited legislation through
 *   enumeration. The constraint exhibits high suppression (0.72) because the
 *   immunity is enforced by jurisdictional exclusion — a claim against vault
 *   contents is dismissed on standing grounds before substantive
 *   constitutional review even begins. The extractiveness (0.58) reflects
 *   that the mechanism benefits the depositing parliament and executive while
 *   harming rights claimants, but the benefit is not maximal extraction
 *   (0.66+) because the coordination gains are genuine: the Schedule did
 *   enable land reform that would have been blocked by courts applying
 *   Article 31's compensation requirements. The suppression trajectory
 *   (rising from 0.65 to 0.80 over 40 years) reflects expanding deposits and
 *   broadening categories of immunized legislation. The extractiveness
 *   trajectory (rising from 0.35 to 0.62, then declining to 0.58 post-Coelho)
 *   reflects the constraint's intensification until Coelho v. State of
 *   Karnataka (2007) reinstituted basic structural rights review for vault
 *   contents. This reading competes with two siblings: the
 *   founders_amending_founders_reading (which emphasizes Nehru's amendment as
 *   part of the founding process, not a permanent vault design), and the
 *   speech_grounds_expansion_reading (which focuses on the First Amendment's
 *   simultaneous narrowing of Article 19(2) free speech protections through
 *   new exceptions for public order and friendly relations).
 *
 * KEY AGENTS:
 *   - Nehru's Government (1951): Founding depositor (institutional/arbitrage) — benefits from immediate passage of land reform without blocking constitutional challenges; creates the vault mechanism itself
 *   - Parliament and Executive (1951–2007): Repeat depositors (institutional/arbitrage) — expanding vault contents to cover tenancy laws, agricultural reorganization, food security statutes, and increasingly unrelated legislation; primary beneficiary of expanding immunity
 *   - Rights Claimants Against Vault Contents: Primary victims (powerless/trapped) — individuals and groups challenging land reform, tenancy, and later statutes face jurisdictional exclusion from courts; no exit from the suppression
 *   - Judicial Review Apparatus (1951–2007): Institutional actor (institutional/arbitrage) — bound to respect vault immunity; performs legitimate constitutional review role while ceding genuine authority over scheduled legislation; maintains appearance of supremacy
 *   - Coelho Coalition (1990–2007): Organized reform movement (organized/constrained) — civil rights organizations, law schools, constitutional scholars challenging vault legitimacy; succeeded in 2007 in reinstating basic structural rights review for scheduled legislation
 *   - Constitutional Founders (1950): Ambedkar and Constituent Assembly — established the text that Nehru's First Amendment modified; frame the reading's competing interpretation with founders_amending_founders_reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_1951__ninth_schedule_immunity_reading, 0.58).
domain_priors:suppression_score(first_amendment_1951__ninth_schedule_immunity_reading, 0.72).
domain_priors:theater_ratio(first_amendment_1951__ninth_schedule_immunity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_1951__ninth_schedule_immunity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_1951__ninth_schedule_immunity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(first_amendment_1951__ninth_schedule_immunity_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_1951__ninth_schedule_immunity_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_1951__ninth_schedule_immunity_reading, "Ninth Schedule Constitutional Immunity (First Amendment 1951 Reading)").
narrative_ontology:topic_domain(first_amendment_1951__ninth_schedule_immunity_reading, "legal/constitutional/india").

domain_priors:requires_active_enforcement(first_amendment_1951__ninth_schedule_immunity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_1951__ninth_schedule_immunity_reading, '2461da03-f2f7-4a0d-9f3c-d10017a581dc').
narrative_ontology:cs_kernel_codification('2461da03-f2f7-4a0d-9f3c-d10017a581dc', formalized).
narrative_ontology:cs_authority_grounding('2461da03-f2f7-4a0d-9f3c-d10017a581dc', lineage).
narrative_ontology:cs_interpretation_layer_present('2461da03-f2f7-4a0d-9f3c-d10017a581dc').
narrative_ontology:cs_reading_relation('2461da03-f2f7-4a0d-9f3c-d10017a581dc', first_amendment_1951__founders_amending_founders_reading, coexists_with).
narrative_ontology:cs_reading_relation('2461da03-f2f7-4a0d-9f3c-d10017a581dc', first_amendment_1951__speech_grounds_expansion_reading, coexists_with).
narrative_ontology:cs_axiom('2461da03-f2f7-4a0d-9f3c-d10017a581dc', foundational, ninth_schedule_permanence).
narrative_ontology:cs_axiom_status(ninth_schedule_permanence, holdable).
narrative_ontology:cs_axiom_grounding('2461da03-f2f7-4a0d-9f3c-d10017a581dc', ninth_schedule_permanence, conventional).
narrative_ontology:cs_axiom('2461da03-f2f7-4a0d-9f3c-d10017a581dc', foundational, enumeration_as_jurisdictional_barrier).
narrative_ontology:cs_axiom_status(enumeration_as_jurisdictional_barrier, overridden).
narrative_ontology:cs_axiom_grounding('2461da03-f2f7-4a0d-9f3c-d10017a581dc', enumeration_as_jurisdictional_barrier, deontological).
narrative_ontology:cs_reference_frame('2461da03-f2f7-4a0d-9f3c-d10017a581dc', vault_architecture_permanence).
narrative_ontology:cs_drift_state('2461da03-f2f7-4a0d-9f3c-d10017a581dc', coelho_judgment_2007, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2461da03-f2f7-4a0d-9f3c-d10017a581dc', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(first_amendment_1951__ninth_schedule_immunity_reading, first_amendment_1951).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_1951__ninth_schedule_immunity_reading, deposited_legislation).
narrative_ontology:constraint_beneficiary(first_amendment_1951__ninth_schedule_immunity_reading, executive_consolidation).
narrative_ontology:constraint_victim(first_amendment_1951__ninth_schedule_immunity_reading, rights_claims_against_vault).
narrative_ontology:constraint_victim(first_amendment_1951__ninth_schedule_immunity_reading, judicial_review_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS CLAIMANTS (SNARE) — Individuals and groups challenging land reform statutes or later deposited legislation locked in the vault have zero exit option. The constraint suppresses judicial review through enumeration: a claim cannot be brought against Schedule Nine contents, regardless of rights violation. The constraint is experienced as pure extraction — exclusion from the courts.
constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFECTED COMMUNITIES (TANGLED ROPE) — Social movement organizations, tenant unions, and land reform beneficiaries experience genuine coordination gains (the Schedule facilitates land redistribution that might otherwise be blocked by courts) alongside extraction (the immunization mechanism suppresses counterclaims and alternative visions of reform). The constraint both enables their interests and forecloses scrutiny of how the enabled legislation actually affects them.
constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENT AND EXECUTIVE (ROPE) — The primary beneficiary. Parliament treats the Ninth Schedule as a coordination mechanism: the vault enables social legislation (land reform, tenancy laws, agricultural reorganization) to be implemented without judiciary blocking it. The extraction runs toward this agent — they capture the power to immunize their legislative agenda from constitutional constraint.
constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL REVIEW APPARATUS PRE-COELHO (PITON) — The courts' review capacity for constitutional rights is substantially performative at the vault boundary. Judges are formally bound to accept Schedule Nine immunity as legitimate constitutional architecture, yet the actual checking function (constitutionality review) is suspended for vault contents. The judiciary maintains the appearance of supremacy over ordinary legislation while ceding genuine immunity to the Schedule — degraded function sustained through inertia.
constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM COALITION (SCAFFOLD) — Civil rights organizations, academic constitutionalists, and litigation NGOs organized around the Coelho v. State of Karnataka (2007) doctrine see the Ninth Schedule immunity as a temporary constitutional arrangement with a sunset. The Coelho judgment reinstates basic structural rights review even for Schedule Nine legislation — the vault is reopened, at least partially. This perspective treats Schedule Nine as a transitional mechanism whose extraction mechanism is being dismantled through coordinated legal challenge.
constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FIXITY VIEW (MOUNTAIN) — From the standpoint of the First Amendment as an immutable constitutional text, the Ninth Schedule is presented as a permanent feature of the constitutional architecture: a built-in mechanism for protecting certain legislation from future judicial challenge. This perspective risks naturalizing what is actually a contingent institutional choice as an inherent property of constitutional amendment power itself.
constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_1951__ninth_schedule_immunity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_amendment_1951__ninth_schedule_immunity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_1951__ninth_schedule_immunity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_amendment_1951__ninth_schedule_immunity_reading, TR),
    TR >= 0.70.

:- end_tests(first_amendment_1951__ninth_schedule_immunity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint benefits the depositing parliament by enabling land reform without rights-based blocking, and it benefits later legislatures by providing a template for immunizing their favored legislation from constitutional scrutiny. The extraction is significant (0.58) because rights claimants are completely excluded from judicial review, but it is not maximal (0.66+) because the land reform coordination function is genuine — those who benefit from the reforms gain real goods, and the parliament's use of the Schedule is not pure predation. The extractiveness trajectory (rising to 0.62 by 1990, then declining to 0.58 post-Coelho) reflects that the constraint's power increased as deposits broadened beyond land reform into tenancy law, agricultural reorganization, and eventually food security and constitutional amendments themselves, but the Coelho doctrine's reinstatement of basic structure review reduced the effective immunity. Suppression (0.72): High. The mechanism suppresses rights claims through enumeration — claims against Schedule Nine legislation are dismissed on jurisdictional grounds without substantive constitutional review. However, suppression is not total (0.80+) because the immunity is formally articulated and challengeable (as Coelho proved), not hidden behind legitimacy claims. The suppression trajectory (rising from 0.65 to 0.80 as deposits expanded, then declining to 0.72 post-Coelho) reflects that more legislation in the vault meant more suppressed claims, and the early period (1951–1970) saw strong internalization of vault legitimacy (suppression through internalized acceptance), but later challenges (1990–2007) externalized the suppression (judges had to dismiss cases explicitly, signaling the lack of substantive legitimacy). Theater ratio (0.35): Low. The Ninth Schedule mechanism is not primarily performative — it is a straightforward jurisdictional carve-out. Courts do not pretend to review vault contents; they simply dismiss on standing grounds. The low theater reflects that the constraint relies on formal authority (enumeration in the Constitution) rather than on ritualized legitimacy performance.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap appears between the beneficiary parliament (rope: coordination benefit without extraction cost) and the trapped rights claimant (snare: pure suppression of review access). The parliament sees the vault as enabling legitimate land reform; the rights claimant sees it as jurisdictional foreclosure. The analytical observer's mountain perspective (viewing the vault as a permanent, immutable constitutional feature) contrasts sharply with the scaffold perspective (viewing the vault as a temporary mechanism dismantled by Coelho). The piton perspective reveals that the judicial review apparatus has degraded function — courts maintain the appearance of constitutional supremacy while ceding actual authority over vault contents. The dramatic shift in the tangled_rope classification for moderate agents (affected communities) across time reflects that the constraint's coordination-to-extraction ratio changed: early land reform (1951–1970) showed stronger coordination gains, but later deposits (1970–2007) showed extraction with weaker coordination rationale.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values follow from structural positions. Parliament as depositor holds institutional power and arbitrage exit — they can shift deposits or seek constitutional amendment. Rights claimants against vault contents hold powerless power and trapped exit — the constraint is enforceable through jurisdictional dismissal, and there is no alternative forum. The court apparatus holds institutional power but constrained exit — they are bound to respect vault immunity, yet their legitimacy depends on appearing as ultimate constitutional arbiters. The Coelho coalition holds organized power and constrained exit — they cannot override vault immunity directly, but organized litigation pressure succeeded in shifting the law. The directionality computation (via the engine's sigmoid f(d)) produces high χ for the powerless/trapped victim agent and low or negative χ for the institutional/arbitrage beneficiary, as expected.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the question 'is this coordination or extraction?' has a time-indexed answer. In 1951, the Ninth Schedule was plausibly coordinate — it solved a coordination problem (enabling land reform without judicial blocking). By 1970–1990, it had become a template for extraction — subsequent deposits broadened far beyond land reform into tenancy, agriculture, food security, and constitutional amendments themselves. By 2007, the constraint had crossed the snare threshold (χ ≥ 0.66) for powerless agents because suppression had intensified (0.80) while the coordination rationale had weakened. Coelho's reinstatement of basic structure review is the Coelho threshold event: the constraint's extractiveness declined (0.62 → 0.58) because the vault was no longer fully sealed. The tangled_rope classification at present reflects that the constraint still coordinates legitimate legislative aims (land reform, tenancy protection) while still suppressing some categories of rights claims — but the suppression is now contestable rather than absolute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vault_boundary_expansion,
    'Is the extractiveness of the Schedule Nine immunity mechanism expanding or contracting over its lifecycle?',
    'Longitudinal analysis of deposited statute frequency and scope (1951–2007); correlation between deposits and failed judicial challenges; document the rate and breadth of legislative categories moved into the vault',
    'If expanding: the constraint is a ratcheting extraction mechanism, increasingly suppressing rights claims across diverse policy domains. If contracting or flat: the extraction mechanism is stable, contained, or already undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vault_boundary_expansion, empirical, 'Trajectory of Ninth Schedule deposits and scope expansion').

omega_variable(
    coelho_reversal_completeness,
    'Does the Coelho doctrine fully reopen the vault or merely crack the seal — allowing basic structural rights review but leaving large domains of immunity intact?',
    'Analysis of post-Coelho judgments: count successful vs blocked constitutional challenges to Schedule Nine legislation; identify which domains remain immunized vs reopened; assess whether the basic structure test preserves immunity for land reform but not for later deposits',
    'If fully reopened: the Schedule Nine constraint transitions from tangled_rope to rope (coordination without extraction). If partially reopened: the constraint remains tangled_rope with reduced but persistent suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coelho_reversal_completeness, empirical, 'Extent of vault reopening under Coelho doctrine').

omega_variable(
    founding_generation_intent_versus_interpretation,
    'Did Nehru''s government intend the Ninth Schedule as a permanent vault, or as a temporary mechanism for First Amendment land reform that later governments misrepresented as settled architecture?',
    'Reconstruction of First Amendment parliamentary debates and committee notes; comparison with founders_amending_founders_reading evidence (the founding as process); historical reconstruction of whether the Schedule was explicitly presented as permanent or as context-dependent',
    'If temporary: this reading loses foundational purchase (the Schedule was never meant to be a constitutional vault). If permanent: the vault authority is rooted in founding intent and Coelho becomes unauthorized judicial revision. If ambiguous: the Coelho doctrine becomes a legitimate interpretive update to an under-specified founding commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_generation_intent_versus_interpretation, conceptual, 'Founding intent regarding permanent vs temporary immunity').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the suppression of vault-content constitutional claims internalized (people stop bringing challenges because they believe immunity is legitimate) versus externally enforced (judges dismiss for lack of standing, with no legitimacy basis)?',
    'Litigation frequency analysis: rates of filed vs attempted constitutional challenges against Schedule Nine legislation; interviews with practicing rights advocates about perceived legitimacy vs perceived futility; comparison of suppression trajectory before and after Coelho judgment (which signals external enforcement is not inevitable)',
    'If mostly internalized: the constraint''s suppression power is legitimacy-based and fragile once that legitimacy is questioned (as Coelho does). If mostly external: the constraint is coercive and persists regardless of legitimacy shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Balance between internalized legitimacy and external enforcement in suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_1951__ninth_schedule_immunity_reading, 1951, 2007).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ninths_theater_t1951, first_amendment_1951__ninth_schedule_immunity_reading, theater_ratio, 1951, 0.28).
narrative_ontology:measurement(ninths_theater_t1970, first_amendment_1951__ninth_schedule_immunity_reading, theater_ratio, 1970, 0.32).

% Extraction over time
narrative_ontology:measurement(ninths_extractiveness_t1951, first_amendment_1951__ninth_schedule_immunity_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement(ninths_extractiveness_t1970, first_amendment_1951__ninth_schedule_immunity_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(ninths_extractiveness_t1990, first_amendment_1951__ninth_schedule_immunity_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(ninths_extractiveness_t2007, first_amendment_1951__ninth_schedule_immunity_reading, base_extractiveness, 2007, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ninths_suppression_t1951, first_amendment_1951__ninth_schedule_immunity_reading, suppression_requirement, 1951, 0.65).
narrative_ontology:measurement(ninths_suppression_t1970, first_amendment_1951__ninth_schedule_immunity_reading, suppression_requirement, 1970, 0.74).
narrative_ontology:measurement(ninths_suppression_t1990, first_amendment_1951__ninth_schedule_immunity_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(ninths_suppression_t2007, first_amendment_1951__ninth_schedule_immunity_reading, suppression_requirement, 2007, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_1951__ninth_schedule_immunity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_1951__ninth_schedule_immunity_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_1951__ninth_schedule_immunity_reading, first_amendment_1951__founders_amending_founders_reading).
narrative_ontology:affects_constraint(first_amendment_1951__ninth_schedule_immunity_reading, first_amendment_1951__speech_grounds_expansion_reading).
narrative_ontology:affects_constraint(first_amendment_1951__ninth_schedule_immunity_reading, coelho_basic_structure_doctrine).
narrative_ontology:affects_constraint(first_amendment_1951__ninth_schedule_immunity_reading, article_19_free_speech_narrowing_1951).

% DUAL FORMULATION NOTE:
% The Ninth Schedule immunity is part of a constraint family originating in the First Amendment (1951). This story tracks the vault-as-permanent-architecture reading. The sibling stories track the amendment-as-process reading and the speech-narrowing reading. All three share the same kernel (the First Amendment text) but disagree on what it means and which element was constitutively important. The Coelho constraint represents the post-2007 dismantling of the vault's immunity status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_1951__ninth_schedule_immunity_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
