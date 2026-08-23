% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Secularism: Affirmative State Duty Against Oppressive Religious Practice
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   In jurisdictions descended from transformative constitutionalism
 *   (paradigmatically India), the reformist reading of secularism holds that
 *   the state bears an affirmative duty to eliminate religious practices that
 *   oppress marginalized groups — caste exclusion from temples, ritual
 *   untouchability, regressive personal law — and that this duty supersedes
 *   community claims of religious autonomy. The arrangement operates through
 *   constitutional text, reform legislation, and a judiciary empowered to
 *   reclassify practices as regulable social custom rather than protected
 *   doctrine. This file instantiates ONLY the reformist reading of the
 *   constitutional_secularism kernel; the strict_neutrality and
 *   principled_intervention readings are separate constraints with their own
 *   epsilon, beneficiary structures, and classifications, linked through
 *   network.affects_constraints. The epsilon referent is the standing
 *   intervention regime as this reading itself assesses it: the reading
 *   affirms rather than discounts the heavy burden the duty imposes on
 *   religious autonomy, so epsilon is authored high. Claimed type and metrics
 *   are independent authored facts: I claim tangled_rope because the
 *   arrangement possesses a genuine coordination function (protection for
 *   members who cannot exit their communities or win internal argument) AND
 *   asymmetric extraction (communal self-governance surrendered to state
 *   organs), sustained by active enforcement.
 *
 * KEY AGENTS:
 *   - scheduled_castes_and_tribes: Primary beneficiary (organized/identity_locked) — gains enforceable access and dignity rights overriding community refusal
 *   - women_within_religious_communities: Primary beneficiary (moderate/constrained) — gains statutory and judicial remedies independent of internal community consent
 *   - religious_conservatives: Primary target (organized/constrained) — bears erosion of communal self-governance across communities
 *   - traditional_religious_authorities: Secondary target (institutional/constrained) — loses administrative and doctrinal control of contested practices
 *   - legislature_and_judiciary: Agenda setter (institutional/arbitrage) — writes, adjudicates, and administers the ameliorative apparatus; accrues jurisdiction with each intervention
 *   - lay_conservative_believers: Excluded voice (powerless/identity_locked) — devotional life restructured by processes they never join
 *   - secular_reform_movements: Secondary beneficiary (moderate/mobile) — collects standing, funding, and agenda influence from the instrument's availability
 *   - comparative_constitutional_scholars: Analytical observer — maps the doctrine across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.72).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.64).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Secularism: Affirmative State Duty Against Oppressive Religious Practice").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, 'e504b5e6-2209-4f5b-9157-751623145f48').
narrative_ontology:cs_kernel_codification('e504b5e6-2209-4f5b-9157-751623145f48', fixed_text).
narrative_ontology:cs_authority_grounding('e504b5e6-2209-4f5b-9157-751623145f48', lineage).
narrative_ontology:cs_interpretation_layer_present('e504b5e6-2209-4f5b-9157-751623145f48').
narrative_ontology:cs_reading_relation('e504b5e6-2209-4f5b-9157-751623145f48', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('e504b5e6-2209-4f5b-9157-751623145f48', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('e504b5e6-2209-4f5b-9157-751623145f48', foundational, autonomy_yields_to_amelioration).
narrative_ontology:cs_axiom_status(autonomy_yields_to_amelioration, holdable).
narrative_ontology:cs_axiom_grounding('e504b5e6-2209-4f5b-9157-751623145f48', autonomy_yields_to_amelioration, deontological).
narrative_ontology:cs_axiom('e504b5e6-2209-4f5b-9157-751623145f48', secondary, closed_community_self_correction_insufficient).
narrative_ontology:cs_axiom_status(closed_community_self_correction_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('e504b5e6-2209-4f5b-9157-751623145f48', closed_community_self_correction_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('e504b5e6-2209-4f5b-9157-751623145f48', transformative_amelioration_baseline).
narrative_ontology:cs_drift_state('e504b5e6-2209-4f5b-9157-751623145f48', contemporary_autonomy_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e504b5e6-2209-4f5b-9157-751623145f48', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes_and_tribes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_within_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, traditional_religious_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, secular_reform_movements).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, transformative_constitutionalism).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, social_welfare_and_reform_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically barred from temple entry, ritual equality, and religious office by caste practice. Constitutional abolition of untouchability and subsequent entry legislation convert their access claims into rights enforceable against their own communities' refusal. Leaving the caste order is not a realistic option — identity follows them across region and congregation — so external legal enforcement is their principal available lever, and its reach determines what they can actually enter and claim.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes_and_tribes, beneficiary,
    organized, generational, identity_locked, national).

% Women subject to gender-exclusionary ritual rules and personal-law provisions administered by their communities. Statutory reform and litigation supply remedies — entry, marriage and divorce protection, inheritance — that do not depend on winning internal argument with community leadership. Exiting the community typically costs family, livelihood, and belonging, so the availability of an external forum is decisive for them.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_within_religious_communities, beneficiary,
    moderate, biographical, constrained, national).

% Practitioners and movements across communities who regard the targeted practices as binding obligation rather than disposable custom. They lose decision-making over their own ritual life piece by piece as legislatures and courts reclassify obligations as regulable social conduct. Their recourse is litigation and electoral mobilization, both of which run through institutions that have repeatedly upheld the ameliorative override; they cannot opt out of the constitutional order that issues it.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives, payer,
    organized, generational, constrained, national).

% Hereditary priesthoods, monastic heads, denominational boards, and temple trusts that administer religious endowments and doctrine. Reform statutes place endowments under government supervision and transfer appointment and ritual decisions to state-appointed bodies where the ameliorative duty attaches. Their consent is not required in those domains, and their administrative role survives mainly in the domains the state has not yet reached.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, traditional_religious_authorities, payer,
    institutional, generational, constrained, national).

% Enacts reform legislation, adjudicates collisions between autonomy claims and ameliorative duties, and administers supervised endowments through dedicated departments and commissioners. Every collision resolved in favor of amelioration enlarges the body of precedent and the administrative apparatus available for the next one. The same organs that apply the standard also define which practices fall under it.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, legislature_and_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Ordinary worshippers whose devotional practice is restructured by statutes and judgments negotiated among political elites, institutional leadership, and litigants. They are rarely parties or consultees; representation runs through leadership that may itself be a losing party. Their attachment to the affected practice is constitutive of daily religious life, and relocation or conversion is not a proportionate response to a changed ritual rule.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, lay_conservative_believers, excluded,
    powerless, biographical, identity_locked, local).

% Ambedkarite, feminist, and civil-liberties organizations that draft reform bills, bring public-interest litigation, and mobilize temple-entry and personal-law campaigns. The availability of the ameliorative instrument gives them standing, funding, and agenda influence disproportionate to their numbers; their organizational fortunes rise with each successful intervention.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, secular_reform_movements, beneficiary,
    moderate, biographical, mobile, national).

% Academic observers tracking how ameliorative-supremacy doctrines operate across jurisdictions — what they displace, whom they protect, and where the line between reformable practice and protected doctrine is drawn. They publish analyses but hold no decision power and no material stake in outcomes.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, legislature_and_judiciary).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that closed religious communities cannot internally remedy practices harming their weakest members: dissenters lack both exit and winning voice, so an external enforcer whose duty overrides communal refusal guarantees a floor of access and protection that no internal mechanism has reliably delivered.
% TRANSFER_FUNCTION: Moves decision-authority over contested religious practices from hereditary and traditional religious authorities to state institutions — legislatures, courts, endowment departments — and converts the suppressed claims of community members into enforceable rights held against their own communities.
% ABSENT_VOICES: Lay conservative believers within each community would object: their devotional life is restructured by litigation and legislation they never joined, and they are represented only obliquely by institutional leadership that is often itself a losing party. Internal gradualists who preferred negotiated community-led reform are likewise pre-empted by judicial resolution.
% DISAPPEARANCE_RATIONALE: If the affirmative-duty doctrine vanished overnight, temple-entry exclusions, caste-barred ritual, and personal-law arrangements currently overridden would revert to community control; access and protections won over seven decades would become contingent on communal consent again; the endowment-administration bureaucracy would lose its mandate; and the precedent base that structures state-religion relations would dissolve.
% FOUNDING_PROBLEM: Post-colonial constitutional consolidation faced oppressive practices embedded in religious self-governance — untouchability, ritual exclusion of castes, regressive personal law — that the new commitment to democratic equality could not tolerate, while religious communities held deep, legitimately rooted traditions of internal autonomy. The reformist reading was built to resolve that collision in favor of the oppressed.
% FOUNDING_PROBLEM_CORROBORATION: Law-commission reports, judicial findings, and independent sociological surveys outside the benefiting parties document continuing caste exclusion in religious institutions and gender-restrictive practices, corroborating that the founding problem persists in modified forms. Conservative scholarship and community testimony dispute the characterization, arguing the residual conflicts concern core doctrine rather than oppression — which is precisely why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72): the doctrine systematically converts internal communal decisions into state decisions, and the converted domain has widened monotonically — from untouchability and temple entry toward personal law, gender-inclusive ritual, and endowment administration. Suppression (0.64) reflects real coercive machinery — criminal enforcement of anti-untouchability law, compliance backed by contempt powers, statutory takeover of endowments — while litigation and electoral channels remain open, which is why it is not higher. Theater ratio (0.32) is moderate-low: most interventions deliver substantive protection, but a growing share is symbolic reform enacted for political credit or enforced unevenly across regions. Accessibility_collapse (0.55): once the supremacy of the ameliorative duty is understood, autonomy-based defenses reliably fail in court, yet alternative interpretive frames survive in legal discourse and electoral politics, so alternatives are narrowed rather than extinguished. Resistance (0.62) is substantial and persistent — institution-led litigation, mass protest at entry points, and political movements for de-control of endowments. All three temporal series run on one shared grid (t=0,15,30,45,60,75); the rising trajectories encode precedent accumulation and enforcement hardening, not oscillation. Receipt surface: the surrendered decision-authority demonstrably accrues to the state seat — each intervention leaves precedent and administrative control behind — so gain_flow names legislature_and_judiciary even though the designed beneficiaries are the protected classes; reversing the arrangement is prohibitively costly for whoever could fix it, given amendment barriers, entrenched precedent, and the political price of stripping existing protections.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats the arrangement is dispossession: obligations experienced as the core of a way of life are reclassified by outsiders as regulable custom, and consent is never sought. From the beneficiary seats the same structure is the only available guarantor of dignity, because exit from caste or community is not a real option and internal voice has historically failed. From the agenda-setter seat it is mandate: each collision resolved in favor of amelioration confirms institutional purpose and enlarges reach. Identity-lock binds both ends: caste and devotional identity are constitutive rather than chosen, making exit unthinkable rather than merely costly — which is exactly why the beneficiary seats cannot self-help and the payer seats cannot walk away. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled castes and tribes and women within religious communities are declared beneficiaries with effectively no exit (identity_locked / constrained), placing them near the full-beneficiary end of the directionality range. Religious conservatives and traditional religious authorities are declared victims with constrained exit, placing them near the full-target end; their organized and institutional power raises the cost of enforcement but does not move their directional position. The legislature and judiciary hold no declared beneficiary or victim status; their relationship is administrator-and-accruer, and the canonical fallback for their power atom approximates this adequately. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate the two institutional actors (traditional authorities derive high d from victim status; state organs take the fallback), and a power-atom-keyed override would flatten exactly that asymmetry rather than sharpen it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — oppressive practices embedded in religious self-governance — remains contested but not dead: caste exclusion and gender-restrictive ritual persist in documented forms, so the arrangement has not outlived its mandate and is not a decayed-inertia candidate. Mandatrophy analysis cuts both ways here: it prevents misreading the arrangement as pure extraction (a pure-extraction reading ignores that the protected classes are real, numerous, and unable to secure the same protection by any other available mechanism), and it prevents misreading it as pure coordination (a pure-coordination reading ignores the jurisdictional ratchet by which each intervention enlarges state control beyond the protection delivered). The tangled_rope claim holds both facts in view; the mismatch consumer can test the claim against computed per-seat types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is the reformist reading of the constitutional_secularism kernel; what structural differences would instantiate under the strict_neutrality_reading or the principled_intervention_reading?',
    'Author each sibling as its own epsilon-invariant constraint story and compare epsilon, victim sets, and enforcement requirements across the family. The disagreement is located in the supersession clause: autonomy never yields (strict_neutrality), yields case-by-case at state discretion (principled_intervention), or yields automatically wherever oppression is found (this reading).',
    'Under strict_neutrality the affirmative duty disappears and the victim set empties (epsilon falls toward negligible); under principled_intervention the duty becomes discretionary and the victim set narrows to case-specific losers (epsilon falls moderately). The classification of THIS file is unaffected — it is valid for this reading only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: one reading of the constitutional_secularism kernel, with sibling readings as separate constraints.').

omega_variable(
    oppression_boundary_dispute,
    'Which religious practices count as ''oppressing marginalized groups'' such that the ameliorative duty attaches, versus core doctrine shielded by autonomy?',
    'Longitudinal coding of judicial holdings separating practices struck down as oppression from practices upheld as essential doctrine, combined with sociological measurement of harm to identifiable classes.',
    'A narrower boundary shrinks the victim set and lowers effective extraction toward principled_intervention levels; a broader boundary generalizes subordination of autonomy and pushes the arrangement toward dependence on coercion alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oppression_boundary_dispute, conceptual, 'Where the line between reformable practice and protected doctrine sits — the load-bearing ambiguity of the reading.').

omega_variable(
    protection_capture_ratchet,
    'Does the accumulating jurisdiction produced by successive interventions function primarily to protect the marginalized or to aggrandize state control over religious institutions?',
    'Outcome tracking: durable access and dignity gains for excluded classes versus expansions of administrative control unaccompanied by measurable protection gains.',
    'If capture dominates, the arrangement drifts toward pure extraction with the protection story as cover; if protection dominates, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_capture_ratchet, empirical, 'Whether the jurisdictional ratchet serves the doctrine''s stated beneficiaries or the state seat.').

omega_variable(
    beneficiary_consent_heterogeneity,
    'Do the declared beneficiary groups uniformly welcome the ameliorative interventions attributed to them, or do substantial internal minorities experience imposed reform as its own harm?',
    'Survey and ethnographic evidence on within-group attitudes toward specific interventions (temple entry, personal-law change, endowment administration).',
    'Substantial internal dissent would mean the arrangement coordinates a contested preference rather than a unified beneficiary demand — lowering measured net benefit and raising the weight of the payer side in per-seat computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_consent_heterogeneity, empirical, 'Whether beneficiary-group consent to the reformist program is homogeneous.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__reformist_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t45, constitutional_secularism__reformist_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement_basis(cons_tr_t45, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_secularism__reformist_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement_basis(cons_tr_t60, observed).
narrative_ontology:measurement(cons_tr_t75, constitutional_secularism__reformist_reading, theater_ratio, 75, 0.32).
narrative_ontology:measurement_basis(cons_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__reformist_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t45, constitutional_secularism__reformist_reading, base_extractiveness, 45, 0.63).
narrative_ontology:measurement_basis(cons_be_t45, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_secularism__reformist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(cons_be_t60, observed).
narrative_ontology:measurement(cons_be_t75, constitutional_secularism__reformist_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement_basis(cons_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__reformist_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t45, constitutional_secularism__reformist_reading, suppression_requirement, 45, 0.54).
narrative_ontology:measurement_basis(cons_su_t45, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_secularism__reformist_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(cons_su_t60, observed).
narrative_ontology:measurement(cons_su_t75, constitutional_secularism__reformist_reading, suppression_requirement, 75, 0.64).
narrative_ontology:measurement_basis(cons_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional secularism' decomposes into three structurally distinct constraints with materially different epsilon: strict_neutrality_reading (negligible burden on religious autonomy; no affirmative duty), principled_intervention_reading (moderate burden; discretionary intervention), and this reformist_reading (highest burden on autonomy; mandatory supersession). Upstream/downstream structure: the strict_neutrality frame supplies the baseline vocabulary of religious freedom that the other two readings modify, and the principled_intervention reading is the historical stepping-stone from which the reformist duty was argued — each upstream claim is cited as partial warrant for the downstream one. Each story links its siblings via affects_constraints; epsilon is stable within each file because each instantiates exactly one reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
