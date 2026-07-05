% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto as Institutional Survival Mechanism Legitimated by Revelation Claim
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the institutional-pragmatism reading of the 1890
 *   Manifesto kernel: the public declaration ending the teaching of plural
 *   marriage is read here as a strategic institutional survival move under
 *   existential federal coercive pressure, with the revelation narrative
 *   functioning as legitimating cover rather than as an independent divine
 *   communication or a pure exogenous forcing. The theater_ratio rises
 *   sharply at and after 1890 because the gap between the public declaration
 *   (practice ended) and the private reality (secret sealings continued to
 *   roughly 1904, per the Reed Smoot hearings record) is treated here as the
 *   central observable — the M-set gap. This reading does not contest that
 *   federal coercion was real (see the sibling exogenous_override_reading)
 *   nor that church leadership sincerely experienced something as revelatory
 *   (see endogenous_reinterpretation_reading); it asserts a third, distinct
 *   structural claim: that the doctrinal framing was deployed instrumentally
 *   to manage both external legitimacy (federal/political) and internal
 *   legitimacy (membership cohesion) simultaneously, and that this dual
 *   function is what makes the constraint tangled_rope rather than a clean
 *   mountain (external law) or a clean rope (pure internal coordination).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.71).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto as Institutional Survival Mechanism Legitimated by Revelation Claim").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '4af7b91c-5e79-4aab-b861-4058d4dca146').
narrative_ontology:cs_kernel_codification('4af7b91c-5e79-4aab-b861-4058d4dca146', formalized).
narrative_ontology:cs_authority_grounding('4af7b91c-5e79-4aab-b861-4058d4dca146', extraction).
narrative_ontology:cs_interpretation_layer_present('4af7b91c-5e79-4aab-b861-4058d4dca146').
narrative_ontology:cs_reading_relation('4af7b91c-5e79-4aab-b861-4058d4dca146', plural_marriage_mandate__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('4af7b91c-5e79-4aab-b861-4058d4dca146', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('4af7b91c-5e79-4aab-b861-4058d4dca146', foundational, doctrinal_narrative_serves_institutional_survival_function).
narrative_ontology:cs_axiom_status(doctrinal_narrative_serves_institutional_survival_function, holdable).
narrative_ontology:cs_axiom_grounding('4af7b91c-5e79-4aab-b861-4058d4dca146', doctrinal_narrative_serves_institutional_survival_function, instrumental).
narrative_ontology:cs_axiom('4af7b91c-5e79-4aab-b861-4058d4dca146', secondary, revelation_claim_and_coercive_causation_are_jointly_operative).
narrative_ontology:cs_axiom_status(revelation_claim_and_coercive_causation_are_jointly_operative, holdable).
narrative_ontology:cs_axiom_grounding('4af7b91c-5e79-4aab-b861-4058d4dca146', revelation_claim_and_coercive_causation_are_jointly_operative, empirically_contingent).
narrative_ontology:cs_reference_frame('4af7b91c-5e79-4aab-b861-4058d4dca146', continuous_revelatory_authority_under_duress).
narrative_ontology:cs_drift_state('4af7b91c-5e79-4aab-b861-4058d4dca146', reed_smoot_hearings_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4af7b91c-5e79-4aab-b861-4058d4dca146', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, monogamous_majority_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_wives).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, plural_families_dissolved_or_hidden).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_post_manifesto_converts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces federal disincorporation of church assets, imprisonment of leaders, and disenfranchisement of the membership under the Edmunds-Tucker Act. Issues the Manifesto declaring an end to the teaching of plural marriage, framed publicly as revelation, while privately preserving the doctrine's standing and authorizing or tolerating new plural sealings for roughly a decade after 1890. Secures amnesty, restored corporate property, and a path to statehood; retains control over the institution's continuity and its own narrative of the event.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership, beneficiary).

% Already in plural marriages, some entered under religious duty they had no realistic way to refuse without social and spiritual excommunication risk. After 1890 they are told their status is now legally and doctrinally suspended, but their households do not dissolve cleanly; many remain in hiding, unrecognized by law, cut off from the legal protections monogamous wives retain, and blamed by outsiders for a practice they were pressed into.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_wives, payer,
    powerless, biographical, trapped, regional).

% Bear the practical cost of the institution's pivot: families forced underground into secret continuation, children born into legally ambiguous status, economic instability from fractured households, and social stigma from both federal authorities and, later, the church's own disavowal of what it had required of them a generation earlier.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, plural_families_dissolved_or_hidden, payer,
    powerless, biographical, trapped, regional).

% Join or remain in the church on the understanding, per the public Manifesto, that plural marriage has ended by revelation. Some are recruited into or enter new plural unions performed secretly by authorized leaders between 1890 and roughly 1904, unaware the public declaration did not fully reflect ongoing practice. Their trust in the institution's candor is the resource being spent.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_post_manifesto_converts, payer,
    powerless, biographical, constrained, national).

% Gains restored citizenship rights, an end to federal raids and property seizure, and a path toward social respectability and statehood for the Utah territory. Benefits from the institution's survival and normalization without bearing the direct costs imposed on plural families.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, monogamous_majority_membership, beneficiary,
    moderate, generational, constrained, national).

% Applies escalating coercive pressure (disincorporation, disenfranchisement, criminal prosecution) that forces the institutional pivot, but has no voice inside the church's own account of the Manifesto as revelation; the coercive role does not appear in the story's own legitimation frame even though it structurally produced the outcome.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Examine the documentary record — post-Manifesto plural marriages, private correspondence, court testimony from the Reed Smoot hearings — showing the gap between the public declaration and the internal practice, and evaluate the several competing accounts of what the 1890 pivot actually was.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, later_historians_and_dissident_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the institution itself: church corporate existence, control over its own membership and property, and the collective's capacity to continue functioning as a body, at a moment when continued open practice of plural marriage would have resulted in the church's legal dissolution.
% TRANSFER_FUNCTION: Moves the cost of institutional survival onto the plural families and their dependents (who absorb dissolution, concealment, and stigma) and onto later converts (who absorb the deception of a public declaration not matched by private practice), while moving the benefit of survival, restored assets, and political rehabilitation to the hierarchy and the broader membership.
% ABSENT_VOICES: The women and children of plural households whose marriages were dissolved, hidden, or simply left unaddressed had no voice in drafting or ratifying the Manifesto; the federal government's coercive role is structurally present but is excluded from the church's own revelation-framed narrative of the event.
% DISAPPEARANCE_RATIONALE: Had the Manifesto not been issued, federal disincorporation would very likely have proceeded to completion, dismantling the church's corporate structure and property holdings; conversely, if the institutional-pragmatism reading of the event were universally accepted rather than the revelation reading, the church's claim to continuous prophetic authority — and the specific legitimacy basis on which later authority claims rest — would be substantially reorganized.
% FOUNDING_PROBLEM: The federal government's escalating legal campaign (Edmunds Act, Edmunds-Tucker Act) threatened the church with total disincorporation, seizure of temples and other property, and permanent disenfranchisement of its members unless plural marriage was publicly renounced.
% FOUNDING_PROBLEM_CORROBORATION: The specific coercive threat (federal disincorporation under Edmunds-Tucker) was resolved by the 1890s and formally closed by Utah statehood in 1896 — attested by federal court records, the Reed Smoot Senate hearings (1904-1907) which examined continued plural marriages after the Manifesto, and independent historians outside the church hierarchy; the church's own institutional histories, by contrast, continue to frame the Manifesto primarily as a still-live revelation rather than a resolved external pressure, which is the corroboration gap this reading identifies.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.40 pre-Manifesto to a peak near 0.75 around 1904 (the Reed Smoot hearings, which exposed the extent of post-Manifesto plural marriages) then eases slightly by 1910 as the institution completes the transition to public monogamy. Theater ratio spikes immediately in 1890 because the declaration's public function (revelation ending the practice) diverges sharply from private practice (continuation), and stays elevated through 1904 as the gap becomes a matter of federal testimony rather than private rumor. Suppression rises steadily as the institution must actively manage both external federal scrutiny and internal secrecy about continuing plural households — a compounding enforcement burden rather than a single coercive act.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, the sequence reads as continuous, providentially-guided institutional stewardship. From the payer seats (plural families, deceived converts), the same sequence reads as an abrupt, unilaterally imposed reallocation of cost with no meaningful voice in the decision. The engine's per-seat computation should reflect that divergence rather than adjudicate which account is correct — that adjudication is exactly what the three sibling readings of this kernel are for.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy sits closest to the beneficiary pole: it authored the declaration, retained institutional control, and captured the restored corporate assets and political rehabilitation. Coerced polygamist wives and their families sit at the target pole: trapped exit options, no voice in the declaration, and the direct cost of concealment or dissolution. Post-Manifesto converts occupy a distinct victim position — not coerced into polygamy but deceived about the institution's own candor, which is itself a directional cost even though their exit options are less constrained than the plural wives'. Monogamous majority membership benefits diffusely (restored citizenship, social normalization) without bearing the concentrated cost, which is why it is coded beneficiary rather than payer despite moderate rather than institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imminent federal disincorporation) is dead by any external corroboration standard — resolved by 1896 statehood — yet the revelation framing of the 1890 Manifesto persists as a live doctrinal claim within the institution's own self-understanding long after the coercive emergency that occasioned it. This reading treats that persistence as the mandatrophy signature: the doctrinal legitimation outlived the survival function it was built to serve, and continues to be invoked (founding_problem_status: dead, but treated internally as still-live) independent of the external pressure's resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincerity_of_revelation_claim,
    'Did church leadership in 1890 subjectively experience the Manifesto as genuine revelation, or did they consciously deploy revelatory language as strategic legitimation while understanding the true driver to be coercive necessity?',
    'Private correspondence, diary entries, and testimony from participants (some available from Reed Smoot hearings and church archives) could indicate whether leaders privately described the decision in providential or pragmatic terms; the strongest evidence is inherently incomplete since sincere belief and strategic convenience are not mutually exclusive and can coexist in a single agent''s mind.',
    'If leadership sincerely believed the revelation while also recognizing its convenient timing, this reading and the endogenous_reinterpretation_reading may both be partially true simultaneously for different agents within the same institution — the kernel model treats these as separate constraints for separate readers, but historical actors may not have experienced them as separate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sincerity_of_revelation_claim, conceptual, 'Whether the revelation claim was subjectively sincere, strategically instrumental, or an inseparable blend of both for the agents who issued it.').

omega_variable(
    post_manifesto_authorization_scope,
    'Were the plural marriages performed after 1890 (documented through 1904) authorized by the church''s highest leadership as official policy, or were they unauthorized actions by individual leaders acting outside sanctioned church policy?',
    'The Reed Smoot hearing record, church court records, and internal correspondence distinguishing centrally sanctioned sealings from individually initiated ones; the Second Manifesto of 1904 itself is evidence that the institution judged some prior continuation to have occurred with insufficient central control.',
    'If centrally authorized, the beneficiary/victim structure implicates the top hierarchy directly in the deception of converts; if decentralized and unauthorized, the extraction is more diffuse and the hierarchy''s culpability for the deceived-converts victim class is reduced, which would lower measured extractiveness for that specific relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_authorization_scope, empirical, 'Whether post-1890 plural marriages were centrally sanctioned institutional policy or decentralized unauthorized continuation.').

omega_variable(
    coalition_potential_of_plural_families,
    'Could the powerless, trapped plural families have organized collective resistance or public disclosure that would have altered the institution''s cost-benefit calculation regarding continued concealment?',
    'Examine documented instances of plural wives who did testify publicly (some did, at legal and social cost) and assess whether coordinated collective testimony, had it occurred, would have shifted federal or internal church policy earlier.',
    'If coalition action was structurally foreclosed (excommunication risk, social isolation, lack of communication networks among geographically dispersed households), the powerlessness is closer to absolute; if some coordination was feasible but simply did not occur, the victim classification should note latent rather than fully exhausted agency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_potential_of_plural_families, empirical, 'Whether coalition power was structurally available to trapped plural family members but unexercised, or genuinely foreclosed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1885, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1885, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1885, 0.2).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.55).
narrative_ontology:measurement(plur_tr_t1895, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1895, 0.65).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1900, 0.62).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.7).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1910, 0.5).

% Extraction over time
narrative_ontology:measurement(plur_be_t1885, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1885, 0.4).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.62).
narrative_ontology:measurement(plur_be_t1895, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1895, 0.7).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.75).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1910, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1885, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1885, 0.3).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(plur_su_t1895, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1895, 0.6).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.74).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1910, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the plural_marriage_mandate kernel, decomposed per the ε-invariance principle because the natural-language label 'the 1890 Manifesto' conflates structurally distinct claims about causation and legitimacy. institutional_pragmatism_reading treats the revelation narrative as instrumentally deployed legitimation for coercion-driven survival (tangled_rope: genuine institutional-survival coordination entangled with asymmetric cost imposed on plural families and deceived converts). exogenous_override_reading treats the event as pure forced abandonment with no independent doctrinal legitimation work being done. endogenous_reinterpretation_reading treats it as genuine, non-instrumental prophetic revelation. Each carries its own ε, beneficiary/victim structure, and type; they are linked here rather than merged because merging would violate ε-invariance — measuring the same historical event under 'was this pragmatic?' versus 'was this coerced?' versus 'was this revelation?' yields three different extraction profiles for what the colloquial label treats as one claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
