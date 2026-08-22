% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Post-1890 Monogamy Enforcement Regime (Coercion-Warrant Reading)
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   Under the coercion-visibility reading of the divine-marriage-command
 *   kernel, the standing arrangement under contest is the post-1890 regime by
 *   which the Latter-day Saint hierarchy binds members to monogamy on an
 *   openly acknowledged warrant: federal coercion and institutional survival
 *   necessity, not new revelation. The 1890 Manifesto and the harder 1904
 *   Second Manifesto are treated as binding acts whose legitimacy derives
 *   from the alternative — disincorporation, property seizure, imprisonment
 *   of leaders — rather than from a superseding command. This file
 *   instantiates ONLY that reading; the continuationist reading (command
 *   doctrinally valid, practice suspended) and the substitutionist reading
 *   (monogamy newly commanded by revelation) are separate constraints linked
 *   through network.affects_constraints. The epsilon referent is the
 *   enforcement arrangement as this reading assesses it: real transfers from
 *   plural families, practitioners, and dissenters, set against a genuine
 *   collective-action rescue of the whole community. Claim and metrics are
 *   authored independently: the claim is tangled_rope (rescue coordination
 *   carrying asymmetric costs, actively enforced); the metrics describe the
 *   arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - first_presidency_leadership: agenda setter (institutional / identity_locked) — administers the post-1890 prohibition and collects institutional survival
 *   - federal_authorities: external agenda setter and incidental beneficiary (institutional / arbitrage) — the coercion source whose demands the arrangement satisfies
 *   - plural_family_households: primary target (moderate / trapped) — bears dissolution, stigma, and legal-insecurity costs
 *   - post_manifesto_plural_marriers: target (moderate / constrained) — disciplined when enforcement hardened after 1904
 *   - fundamentalist_successors: target (powerless / identity_locked) — expelled for holding the original command binding
 *   - rank_and_file_membership: net beneficiary with diffuse payment (organized / constrained) — trades epistemic deference for legal peace
 *   - historians_of_mormonism: analytical observer — sees the full documentary structure without adopting any reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.66).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Post-1890 Monogamy Enforcement Regime (Coercion-Warrant Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, 'cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a').
narrative_ontology:cs_kernel_codification('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', fixed_text).
narrative_ontology:cs_authority_grounding('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', lineage).
narrative_ontology:cs_interpretation_layer_present('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a').
narrative_ontology:cs_reading_relation('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_axiom('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', foundational, coercion_valid_doctrinal_input).
narrative_ontology:cs_axiom_status(coercion_valid_doctrinal_input, holdable).
narrative_ontology:cs_axiom_grounding('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', coercion_valid_doctrinal_input, instrumental).
narrative_ontology:cs_axiom('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', foundational, manifesto_binding_absent_new_revelation).
narrative_ontology:cs_axiom_status(manifesto_binding_absent_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', manifesto_binding_absent_new_revelation, conventional).
narrative_ontology:cs_reference_frame('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', circumstance_conditioned_revelation).
narrative_ontology:cs_drift_state('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', post_smoot_hearings_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf2d5e8b-78e0-4c3a-bcd4-16affcdcdf3a', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, first_presidency_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, federal_authorities).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, plural_family_households).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, post_manifesto_plural_marriers).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, fundamentalist_successors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_necessity).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, manifesto_binding_without_superseding_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets, announces, and administers the post-1890 rule barring new plural marriages: issues the public declarations, answers federal inquiries, screens candidates for temple ordinances, and convenes disciplinary councils for violations. Gains corporate survival, retained property, Utah statehood, and an undiminished claim to preside. Cannot walk away from the arrangement without dissolving the institution it is constituted by, and cannot repudiate the 1890 declarations without reopening federal jeopardy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, first_presidency_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).

% Congress, the Justice Department, and the courts demanded cessation of plural marriage as the price of corporate existence, statehood, and amnesty. They set the terms the arrangement satisfies, verify compliance through hearings and prosecutions, and collect obedience to federal marriage law. Their leverage is sovereign; they face no comparable pressure in return.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, federal_authorities, beneficiary).

% Ordinary members receive legal normalization, statehood's civic and economic dividends, and relief from prosecution risk. They pay by accepting a reversal of a practice their own revelations commanded, on a stated warrant they are asked not to examine closely, and by extending deference to leaders who acknowledge the shift was compelled. Leaving means forfeiting community, family networks, and the salvation framework they hold.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership, payer).

% Households sealed under the pre-1890 command. Husbands served prison terms and went underground in the 1880s; after 1890 their existing covenants lost institutional protection, new sealings were barred, and wives and children carried stigma and legal insecurity. Dissolving the families contradicts their most solemn commitments; maintaining them invites discipline.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, plural_family_households, payer,
    moderate, biographical, trapped, regional).

% Members who entered new plural marriages between 1890 and the mid-1900s, many in the Mormon colonies of Mexico and Canada, some with senior leaders' knowledge. When enforcement hardened after 1904 they faced resignation demands, disciplinary councils, and excommunication. Their options were emigration, concealment, or compliance — each costly.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, post_manifesto_plural_marriers, payer,
    moderate, biographical, constrained, continental).

% Members who concluded the 1843 command remained in force and the 1890 declarations lacked authority. Organized into separate sects after expulsion, they bear criminalization, social ostracism, and loss of temple and fellowship standing. Their self-understanding is built on fidelity to the original command; abandoning it would unmake who they are, so they persist outside the main body.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, fundamentalist_successors, payer,
    powerless, generational, identity_locked, regional).

% Academic and independent researchers who assemble the documentary record — council minutes, private journals, congressional testimony — and publish accounts of what was said publicly and privately about the 1890 declarations. They hold no stake in the arrangement's persistence and can describe all three readings without adopting any.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, historians_of_mormonism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, first_presidency_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of moving a covenant community from an outlawed practice to legal existence: one synchronized cessation preserves corporate continuity, property, and citizenship for all, where unilateral defection by any family would invite federal escalation against everyone.
% TRANSFER_FUNCTION: Moves compliance cost from the institution — its charter, property, and leadership liberty — onto plural families, would-be practitioners, and dissenters; moves epistemic deference from the membership to the hierarchy; moves legal recognition and statehood benefits to the whole community.
% ABSENT_VOICES: The plural wives and children of existing households had no seat in the councils that produced the 1890 declarations; the position later held by the fundamentalist successors (the command stands) was voiced only by individuals later expelled; the rank and file ratified by silence. Federal prosecutors were present only as external pressure, never as negotiating parties.
% DISAPPEARANCE_RATIONALE: Corporate charter, retained property, the Utah statehood settlement, temple standards, and the main body's boundary against plural-marriage movements all presuppose the regime; overnight removal would reopen federal prosecution exposure, revive succession disputes over the 1843 command, and rearrange both the main church and its offshoots.
% FOUNDING_PROBLEM: Federal destruction of the church — disincorporation, property confiscation, imprisonment of leaders, disenfranchisement of members — triggered by the practice of plural marriage commanded by the 1843 revelation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the text and legislative history of the Edmunds-Tucker Act, congressional debate records, contemporaneous non-Mormon press, and later secular historiography all attest that the declarations' proximate cause was federal coercion; private statements of Woodruff to associates, reported by contemporaries and preserved in diaries, corroborate the necessity framing against the public revelatory framing. No corroborating source outside the beneficiary set attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.66: substantial transfers decoupled from consent — existing covenants stripped of institutional protection, practitioners disciplined, dissenters expelled — but bounded by the genuine survival payoff delivered to every seat, including the payers' co-religionists. Suppression 0.72, authored raw and unscaled: persistence depends on active internal machinery (temple-interview screening, disciplinary councils, the resignations of apostles Taylor and Cowley, later expulsions) layered on the federal statute doing external suppression. Theater 0.30: enforcement was substantially theatrical 1890-1904 (public compliance while colony sealings continued), became functional under Smoot-hearings pressure, and retains a ceremonial residue in periodic public readings and reaffirmations. Accessibility collapse 0.52: alternatives — emigration colonies, underground practice, schism — narrowed but never vanished; fundamentalism persists as a costly exit. Resistance 0.58: clandestine marriages, apostolic reluctance, organized successor movements. Identity-lock runs on both flanks: the presidency's exit is locked in the institutional sense (the organization has become its function, so the administrator cannot cheaply revise what it administers), and the successors' lock is doctrinal-relational (their self-concept is constituted by fidelity to the command). The measurement series share one seven-point grid; the 1890-1904 lull is enforcement decay and the post-1904 rise an enforcement ratchet — a decay-ratchet arc, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency's seat the arrangement is the decision that saved everything it stewards — coordination under duress, regrettable but necessary; computed from its beneficiary position and identity-locked exit, extraction damps toward subsidy-of-survival. From the plural-family and dissenter seats the same acts are uncompensated transfers — covenants voided, fellowship withdrawn — computing as heavily extractive. The federal seat sees neither: simple statutory compliance purchased at acceptable cost. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: first_presidency_leadership (collects survival, retained property, consolidated authority), rank_and_file_membership (legal peace and statehood dividends, paying diffuse epistemic deference through its secondary payer position), federal_authorities (collects statutory compliance). Victim declarations: plural_family_households, post_manifesto_plural_marriers, fundamentalist_successors — with exits trapped, constrained, and identity_locked respectively, pinning each toward the full-target end of d. The derivation chain from these declarations plus exit options produces the intended spread without correction: no two same-power seats needed differentiating beyond what role and exit already encode, so no directionality_overrides entries are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — existential federal destruction of the corporation — died with the 1893 amnesty and 1896 statehood, yet the arrangement not only persisted but hardened (Second Manifesto, expulsions). Authored honestly: founding_problem_status=dead against disappearance_verdict=world_rearranges, the mismatch the battery flags as zombie/capture signal. The genealogy prevents two mislabels: reading the regime as pure coordination (it rescued everyone, therefore benign) ignores that its costs fell on seats with no exit; reading it as pure extraction (coerced capitulation, therefore predatory) ignores that the rescue was real and universal. What remains after the founding problem died is boundary maintenance — the prohibition now polices the line between the main body and plural-marriage movements — a function no seat consciously chose and none profits from enough to administer as rent, but which the administrator could change only at prohibitive cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warrant_generalization_risk,
    'Does the admitted non-revelatory warrant stay confined to the 1890 and 1904 declarations, or does it generalize into precedent under which any binding doctrinal change may rest on institutional necessity?',
    'Track the stated warrants of subsequent major doctrinal decisions and governing-council discussions; look for explicit citation of the Manifesto episode as precedent for necessity-based change.',
    'If generalized, the hierarchy''s authority migrates from lineage toward pragmatic self-preservation, triggering the legitimacy crisis this reading anticipates and shifting the classification of every downstream commitment-system constraint in the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrant_generalization_risk, conceptual, 'Whether coercion-warranted doctrinal change remains exceptional or becomes precedential.').

omega_variable(
    operative_warrant_documentary,
    'Did the authority structure actually operate on the coercion warrant, or is the coercion-visibility account a retrospective reconstruction layered over a leadership that framed the shift as revelatory?',
    'Archival work on council minutes, private journals of Woodruff, Cannon, and Smith, and contemporaneous correspondence, distinguishing public framing from operative deliberation.',
    'If the operative warrant was consistently revelatory, this reading describes an analyst''s arrangement rather than the historical one, and its epsilon and victim structure attach to a counterfactual referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_warrant_documentary, empirical, 'Public revelatory framing versus private necessity deliberation as the operative warrant.').

omega_variable(
    lull_period_enforcement_status,
    'Were the plural marriages contracted between 1890 and 1904 authorized exceptions that lowered the arrangement''s real burden, or evidence that the prohibition was initially maintained largely for show?',
    'Enumerate post-1890 sealings from colony and temple records with authorization chains; compare against contemporaneous disciplinary activity.',
    'Reshapes the extraction and theater trajectories across the lull and re-dates any drift from coordination-dominant to enforcement-dominant operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lull_period_enforcement_status, empirical, 'Status of the 1890-1904 enforcement lull: exception or theatricality.').

omega_variable(
    dissenter_exit_constitution,
    'Is the fundamentalist successors'' inability to leave constitutive (identity formed through fidelity to the command before expulsion) or reactive (identity formed through the experience of expulsion)?',
    'Compare pre- and post-expulsion writings of successor-community founders for whether command-fidelity predates the institutional rupture.',
    'Constitutive lock raises the arrangement''s effective hold on that seat beyond what formal discipline alone measures; reactive lock lowers it and shifts weight to the expulsion events themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dissenter_exit_constitution, empirical, 'Origin of the successor communities'' identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1890, 1935).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.45).
narrative_ontology:measurement_basis(divi_tr_t1890, observed).
narrative_ontology:measurement(divi_tr_t1897, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1897, 0.5).
narrative_ontology:measurement_basis(divi_tr_t1897, observed).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1904, 0.35).
narrative_ontology:measurement_basis(divi_tr_t1904, observed).
narrative_ontology:measurement(divi_tr_t1911, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1911, 0.25).
narrative_ontology:measurement_basis(divi_tr_t1911, observed).
narrative_ontology:measurement(divi_tr_t1918, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1918, 0.22).
narrative_ontology:measurement_basis(divi_tr_t1918, observed).
narrative_ontology:measurement(divi_tr_t1926, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1926, 0.28).
narrative_ontology:measurement_basis(divi_tr_t1926, observed).
narrative_ontology:measurement(divi_tr_t1935, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1935, 0.3).
narrative_ontology:measurement_basis(divi_tr_t1935, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement_basis(divi_be_t1890, observed).
narrative_ontology:measurement(divi_be_t1897, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1897, 0.5).
narrative_ontology:measurement_basis(divi_be_t1897, observed).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1904, 0.58).
narrative_ontology:measurement_basis(divi_be_t1904, observed).
narrative_ontology:measurement(divi_be_t1911, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1911, 0.63).
narrative_ontology:measurement_basis(divi_be_t1911, observed).
narrative_ontology:measurement(divi_be_t1918, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1918, 0.64).
narrative_ontology:measurement_basis(divi_be_t1918, observed).
narrative_ontology:measurement(divi_be_t1926, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1926, 0.65).
narrative_ontology:measurement_basis(divi_be_t1926, observed).
narrative_ontology:measurement(divi_be_t1935, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1935, 0.66).
narrative_ontology:measurement_basis(divi_be_t1935, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement_basis(divi_su_t1890, observed).
narrative_ontology:measurement(divi_su_t1897, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1897, 0.42).
narrative_ontology:measurement_basis(divi_su_t1897, observed).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1904, 0.6).
narrative_ontology:measurement_basis(divi_su_t1904, observed).
narrative_ontology:measurement(divi_su_t1911, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1911, 0.72).
narrative_ontology:measurement_basis(divi_su_t1911, observed).
narrative_ontology:measurement(divi_su_t1918, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1918, 0.74).
narrative_ontology:measurement_basis(divi_su_t1918, observed).
narrative_ontology:measurement(divi_su_t1926, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1926, 0.73).
narrative_ontology:measurement_basis(divi_su_t1926, observed).
narrative_ontology:measurement(divi_su_t1935, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1935, 0.72).
narrative_ontology:measurement_basis(divi_su_t1935, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Manifesto' decomposes into three structurally distinct constraints differing in the warrant of binding force: this coercion-visibility reading (warrant = acknowledged coercion and survival necessity), the continuationist reading (warrant = prudential suspension of a still-valid command), and the substitutionist reading (warrant = new superseding revelation). Their epsilon values differ because victim sets and deception costs differ: this reading prices open acknowledgment of non-revelatory grounds, the substitutionist reading prices a revelation claim, the continuationist prices indefinite suspension. The continuationist reading supplies the duress premise this reading radicalizes into acknowledgment; the substitutionist reading competes for the same compliance this reading secures. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
