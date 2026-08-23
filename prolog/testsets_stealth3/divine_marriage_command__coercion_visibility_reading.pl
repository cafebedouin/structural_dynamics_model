% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Post-Manifesto Monogamy Binding — Coercion-Visibility Reading
 *   domain: religious_authority/political_theology
 *
 * SUMMARY:
 *   Between 1882 and 1890 the United States dismantled the legal existence of
 *   the Church of Jesus Christ of Latter-day Saints over plural marriage: the
 *   Edmunds Act and Edmunds-Tucker Act disincorporated the church, escheated
 *   its property, disfranchised polygamous men, and filled territorial
 *   prisons with plural husbands. Wilford Woodruff's October 1890 Manifesto
 *   (Official Declaration 1) announced the cessation of plural marriage; Utah
 *   statehood followed in 1896, property was restored, and the seating of
 *   Senator Reed Smoot in 1907 closed the national quarrel. This story
 *   instantiates the coercion-visibility reading of the
 *   divine_marriage_command kernel: the Manifesto is an acknowledged response
 *   to federal coercion, and its theological legitimacy derives from
 *   institutional survival necessity rather than new revelation. The epsilon
 *   referent is the standing arrangement under contest — the binding of
 *   Latter-day Saint marital practice to post-Manifesto monogamy as enforced
 *   ecclesiastically, 1890-1910 — assessed by this reading's own lights,
 *   never by the arrangement any sibling reading would put in its place.
 *   Claim and metrics are independent authored facts: the constraint is
 *   CLAIMED as tangled_rope (genuine survival coordination carrying
 *   asymmetric extraction); the metrics describe its actual operation. KEY
 *   AGENTS (by structural relationship): - first_presidency_council:
 *   Agenda-setter (institutional/constrained) — issued and now administers
 *   the Manifesto; cannot repudiate it without unravelling its own authority
 *   - lds_church_corporation: Primary beneficiary (institutional/arbitrage) —
 *   collects survival, restored property, statehood, and consolidated
 *   discipline - federal_justice_establishment: Secondary beneficiary
 *   (institutional/mobile) — obtained its eradication objective by delegating
 *   enforcement to the extracted institution - monogamous_member_majority:
 *   Incidental beneficiary (organized/identity_locked) — received statehood
 *   and respectability; supplied the loyalty that made enforcement durable -
 *   plural_family_wives_and_children: Primary target
 *   (powerless/identity_locked) — bore dissolution, imprisoned husbands, and
 *   lifelong stigma - post_manifesto_practitioners: Secondary target
 *   (moderate/constrained) — disciplined, excommunicated, or driven to
 *   Mexican and Canadian colonies - restorationist_dissenters: Excluded voice
 *   (moderate/constrained) — read the Manifesto as capitulation; answered
 *   with discipline rather than argument - academic_historians: Analytical
 *   observer (analytical/analytical) — reconstructs the documentary record no
 *   seat fully controls
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.72).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.7).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Post-Manifesto Monogamy Binding — Coercion-Visibility Reading").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '4f19632e-0414-488c-9693-81228b361524').
narrative_ontology:cs_kernel_codification('4f19632e-0414-488c-9693-81228b361524', fixed_text).
narrative_ontology:cs_authority_grounding('4f19632e-0414-488c-9693-81228b361524', lineage).
narrative_ontology:cs_interpretation_layer_present('4f19632e-0414-488c-9693-81228b361524').
narrative_ontology:cs_reading_relation('4f19632e-0414-488c-9693-81228b361524', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('4f19632e-0414-488c-9693-81228b361524', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('4f19632e-0414-488c-9693-81228b361524', foundational, survival_necessity_legitimates_doctrine).
narrative_ontology:cs_axiom_status(survival_necessity_legitimates_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4f19632e-0414-488c-9693-81228b361524', survival_necessity_legitimates_doctrine, instrumental).
narrative_ontology:cs_axiom('4f19632e-0414-488c-9693-81228b361524', foundational, coercion_response_binding_absent_revelation).
narrative_ontology:cs_axiom_status(coercion_response_binding_absent_revelation, holdable).
narrative_ontology:cs_axiom_grounding('4f19632e-0414-488c-9693-81228b361524', coercion_response_binding_absent_revelation, conventional).
narrative_ontology:cs_reference_frame('4f19632e-0414-488c-9693-81228b361524', survival_necessity_legitimated_authority).
narrative_ontology:cs_drift_state('4f19632e-0414-488c-9693-81228b361524', post_smoot_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f19632e-0414-488c-9693-81228b361524', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, lds_church_corporation).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, federal_justice_establishment).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, monogamous_member_majority).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, plural_family_wives_and_children).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, post_manifesto_practitioners).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, prophetic_prudence_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_continuity_precedence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto as federal marshals seized church property and imprisoned plural husbands. Publicly attributed the act to divine direction; privately, Woodruff cited the impending loss of the temples and the church's legal existence. After 1904 the presidency and apostolic quorum administered escalating enforcement — temple-interview interrogations, disciplinary councils, mission recalls abroad. The act that saved the institution is now the act they cannot repudiate without unravelling their own authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, first_presidency_council, agenda_setter,
    institutional, generational, constrained, continental).

% Received the settlement's proceeds: restored legal existence, return of property escheated under the 1887 act, Utah statehood in 1896, and enlarged national standing sealed by the seating of Senator Smoot in 1907. Its continuity is the arrangement's clearest gain, and it converted external pressure into consolidated internal discipline.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, lds_church_corporation, beneficiary,
    institutional, generational, arbitrage, continental).

% Prosecuted the anti-polygamy campaign for a generation at great expense; after 1890 it obtained compliance at near-zero marginal cost by letting ecclesiastical discipline carry the load, keeping the statutes on the books as backstop while church courts did the daily work.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_justice_establishment, beneficiary,
    institutional, biographical, mobile, national).

% The large majority of members who never practiced plural marriage received statehood, an end to disfranchisement and raiding, and restored social respectability. They sustained the Manifesto retroactively at the October 1890 conference and supplied the tithing and cultural loyalty that made enforcement durable. Their faith, community, and civic standing are fused; leaving would mean losing all three at once.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, monogamous_member_majority, beneficiary,
    organized, biographical, identity_locked, continental).

% Wives in plural marriages bore the sharpest costs: husbands imprisoned or driven underground, households divided, sealings and inheritance thrown into confusion, and a lifetime of stigma as the practice their faith had sanctified became unspeakable. Few had any voice in the decision; the conference that sustained the Manifesto contained scarcely any of them. Leaving the faith meant losing the entire community that gave their lives meaning, so most stayed and absorbed the costs in place.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, plural_family_wives_and_children, payer,
    powerless, biographical, identity_locked, regional).

% Men who performed or entered plural marriages after 1890 — some quietly authorized by leaders before 1904, some defiant — faced a hardening apparatus: mission recalls, presidency councils after the Second Manifesto, excommunication. Two apostles resigned under pressure in 1905. Colonies in Mexico and Canada offered partial refuge; the price was distance from temples, family networks, and the community that defined them.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, post_manifesto_practitioners, payer,
    moderate, biographical, constrained, continental).

% Members who read the Manifesto as capitulation rather than command gathered at the margins — denied pulpits, disciplined when vocal, eventually organizing outside the communion altogether. They would have argued that coercion cannot supply doctrinal authority; they were met with exclusion rather than hearing.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, restorationist_dissenters, excluded,
    moderate, biographical, constrained, regional).

% Scholars inside and outside the tradition reconstruct the documentary record — Woodruff's private justifications, court filings, Smoot-hearing testimony — and publish accounts the institution neither controls nor fully embraces. They neither gain nor lose standing under the arrangement; their seat is analytical.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, academic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, lds_church_corporation).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated a simultaneous, community-wide withdrawal from a criminalized religious practice: by acting once, centrally, and unanimously, the institution aligned thousands of members with federal law, ending a campaign of raids, imprisonments, and disfranchisement that scattered individual defiance could not have survived.
% TRANSFER_FUNCTION: Moved survival costs from the institution (facing dissolution and property seizure) onto plural-marriage families (bearing dissolution of households, concealment, imprisoned husbands, stigma), and moved legitimacy inward: external federal pressure was converted into internal covenant obligation, so that compliance with the state arrived experienced as obedience to the church.
% ABSENT_VOICES: The plural wives and children who bore the sharpest costs had no seat: the Manifesto was issued by the First Presidency and sustained retroactively by a conference in which they were scarcely represented, and no woman in a plural marriage was consulted on its terms. Practitioners who continued after 1890 were answered with discipline rather than hearing, and dissenters who called the Manifesto capitulation were eventually organized out of the communion. Federal prosecutors set the terms unilaterally; the negotiated surface was far narrower than the affected population.
% DISAPPEARANCE_RATIONALE: If the Manifesto-binding vanished overnight, the doctrinal settlement would unravel in both directions at once: continuationist factions would treat the closure as lifted and resume the practice where statutes still forbid it, inviting renewed prosecution; mainstream and substitutionist factions would confront the claim that a century of monogamous doctrine rested on a revocable prudential act; the fundamentalist communities would be vindicated as the faithful remnant. Schism, legal exposure, and a wholesale renegotiation of prophetic authority would follow — the arrangement, not the world, is what holds these apart.
% FOUNDING_PROBLEM: An existential federal siege: under the Edmunds and Edmunds-Tucker Acts the church faced disincorporation, escheatment of its property, threatened confiscation of its temples, mass imprisonment of plural husbands, and the political extinction of its community — a coercion campaign designed to destroy the institution unless it abandoned the practice a prior revelation had commanded.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the federal court record (Late Corporation of the Church of Jesus Christ of Latter-day Saints v. United States, 136 U.S. 1 (1890)) documents the coercive apparatus in the government's own filings; the Senate committee record of the Smoot hearings documents the national settlement's terms; non-Latter-day Saint historians reconstruct Woodruff's private survival justifications against the public revelatory framing; and the fundamentalist dissenters — who reject the Manifesto's authority precisely because they agree it was coercion-driven — corroborate the causal account from the position of greatest hostility to the arrangement.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.72 at interval end) because the settlement's costs concentrated on the smallest, least powerful seat — plural families whose covenants the faith itself had sanctified — while the gains accrued to the institution as corporate continuity. Suppression (0.70) is the mature ecclesiastical enforcement machine: temple-recommend interrogations, presidency councils after the 1904 Second Manifesto, mission recalls, excommunication. Suppression is a raw structural property, unscaled by power or scope; extractiveness, by contrast, is scaled by directionality and continental scope in the engine's computation. Theater (0.40) reflects the revelatory framing maintained over an acknowledged coercive causation, inflating mid-interval (0.48 at the Smoot hearings) when public testimony contradicted private practice, then settling as enforcement became functional again — a scandal-driven peak, not a reinforcement cycle. Accessibility collapse (0.65): alternatives — colonization abroad, quiet nonconformity, embryonic fundamentalism — survived only at severe cost. Resistance (0.40): practitioner persistence, the 1905 resignation of two apostles under pressure, and a dissenting witness that never fully ceased. The three temporal series share one six-point grid (1890-1910 at four-year steps), each metric authored at every point. Coalition check: the victim seats were geographically scattered, theologically divided between loyalty and defiance, and their communication channels ran through the enforcing institution — the coalition that might have contested the settlement never assembled.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute opposite classifications from identical structure. From the First Presidency's position the Manifesto is the coordination triumph that saved the church — temples, property, and covenant community intact. From the plural families' position the same act is the instrument that dissolved their marriages under external duress relayed through the authority they trusted most. The monogamous majority experiences a third thing: a boundary line that purchased respectability at no personal cost. The engine derives these divergences from the structural data (roles, power, exit); the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the corporation collected the settlement's entire surplus (low d); the federal establishment converted a generation of expensive prosecution into delegated compliance at near-zero marginal cost (low d); the monogamous majority gained statehood and standing while bearing only diffuse identity costs (low-to-moderate d). Targets sit near the full-target end: plural wives and children bore dissolution and stigma with identity-locked exit (high d, amplified by the lock); post-Manifesto practitioners bore discipline and exile with only colonial refuge (high d, moderately amplified by constrained exit). No directionality overrides are needed: the beneficiary/victim declarations plus exit options already separate the institutional beneficiaries from the trapped payers, including the two same-power institutional actors (church corporation and federal establishment), whose opposed relationships the declarations capture directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two mislabels. A pure-snare reading would erase the genuine coordination: without the Manifesto, disincorporation and property escheatment were imminent (Late Corporation v. United States, 1890), and the settlement solved a real collective-action problem. A pure-rope reading would erase the victims: the October 1890 conference sustained the Manifesto retroactively, and the women whose marriages it dissolved had no seat in the room. The hybrid holds both. Mandatrophy: the founding problem — the federal survival siege — died with statehood (1896) and the Smoot settlement (1907), yet the arrangement persists as identity boundary and doctrinal settlement; the R5 mismatch (founding status dead x disappearance verdict world_rearranges) flags the residue honestly rather than laundering it as either eternal doctrine or finished business.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the divine_marriage_command kernel; would instantiating the continuationist or substitutionist reading instead change the victim set, the legitimacy exposure, or the computed type?',
    'Comparative factional history: which reading each community adopted, what happened to its dissenters, and whether the set of authorized plural marriages reopened anywhere after 1890.',
    'A continuationist instantiation keeps plural-practitioner communities inside the faith boundary (different victims, open authorization, lower closure); a substitutionist instantiation attributes the shift wholly to revelation and removes the legitimacy-crisis exposure this reading carries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame location: one of three readings of the divine marriage command kernel.').

omega_variable(
    legitimacy_crisis_exposure,
    'If the authority structure explicitly admitted non-revelatory grounds for the 1890 shift, would prophetic authority absorb the admission or cascade into broader erosion?',
    'Track official acknowledgments (institutional essays conceding political pressure), member-belief surveys, and factional recruitment following each disclosure.',
    'Absorption stabilizes this reading as honest self-understanding; cascade would push the lineage authority toward theatrical maintenance as belief in the revelatory framing fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_crisis_exposure, empirical, 'Whether acknowledged coercion grounds stabilize or erode lineage authority.').

omega_variable(
    suppression_source_partition,
    'How much of the measured suppression is federal-structural versus ecclesiastical-internal, and how does the mix shift across the interval?',
    'Separate the enforcement records: federal prosecutions, seizures, and disfranchisement versus church disciplinary councils, temple-recommend gatekeeping, and mission instructions.',
    'If post-1904 suppression is predominantly ecclesiastical, the arrangement is self-maintaining and effective extraction concentrates on internal targets; if federal force remained the binding agent, the church functions as a transmission belt and attribution shifts toward the federal principal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_source_partition, empirical, 'Partition of suppression between federal and ecclesiastical sources.').

omega_variable(
    survival_threat_magnitude,
    'Was the existential threat real, or did leadership magnify it to legitimate an unpopular retreat?',
    'Counterfactual legal-political historiography: could the church have survived continued resistance through asset dispersal, civil disobedience, or wholesale foreign relocation, as some contemporaries urged?',
    'An overstated threat thins the survival-necessity legitimacy toward cover story and drifts the classification toward pure extraction; a genuine threat anchors the coordination half of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_threat_magnitude, empirical, 'Magnitude of the founding threat underlying the survival-necessity claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmcmd_coercion_vis_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_tr_t0, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_tr_t4, divine_marriage_command__coercion_visibility_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_tr_t4, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_tr_t8, divine_marriage_command__coercion_visibility_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_tr_t8, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_tr_t12, divine_marriage_command__coercion_visibility_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_tr_t12, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_tr_t16, divine_marriage_command__coercion_visibility_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_tr_t16, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_tr_t20, divine_marriage_command__coercion_visibility_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(dmcmd_coercion_vis_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_be_t0, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_be_t4, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_be_t4, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_be_t8, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_be_t8, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_be_t12, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_be_t12, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_be_t16, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_be_t16, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_be_t20, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(dmcmd_coercion_vis_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_su_t0, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_su_t4, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_su_t4, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_su_t8, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_su_t8, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_su_t12, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_su_t12, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_su_t16, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_su_t16, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_su_t20, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_su_t20, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1890, tn=1910
narrative_ontology:measurement(dmcmd_coercion_vis_grid_01, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(class), 1890, 0.45).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_01, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_02, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(class), 1910, 0.7).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_02, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_03, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(individual), 1890, 0.4).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_03, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_04, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(individual), 1910, 0.65).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_04, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_05, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(organizational), 1890, 0.55).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_05, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_06, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(organizational), 1910, 0.8).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_06, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_07, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(structural), 1890, 0.75).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_07, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_08, divine_marriage_command__coercion_visibility_reading, accessibility_collapse(structural), 1910, 0.85).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_08, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_09, divine_marriage_command__coercion_visibility_reading, resistance(class), 1890, 0.6).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_09, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_10, divine_marriage_command__coercion_visibility_reading, resistance(class), 1910, 0.45).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_10, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_11, divine_marriage_command__coercion_visibility_reading, resistance(individual), 1890, 0.5).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_11, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_12, divine_marriage_command__coercion_visibility_reading, resistance(individual), 1910, 0.5).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_12, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_13, divine_marriage_command__coercion_visibility_reading, resistance(organizational), 1890, 0.45).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_13, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_14, divine_marriage_command__coercion_visibility_reading, resistance(organizational), 1910, 0.35).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_14, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_15, divine_marriage_command__coercion_visibility_reading, resistance(structural), 1890, 0.35).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_15, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_16, divine_marriage_command__coercion_visibility_reading, resistance(structural), 1910, 0.15).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_16, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_17, divine_marriage_command__coercion_visibility_reading, stakes_inflation(class), 1890, 0.65).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_17, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_18, divine_marriage_command__coercion_visibility_reading, stakes_inflation(class), 1910, 0.6).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_18, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_19, divine_marriage_command__coercion_visibility_reading, stakes_inflation(individual), 1890, 0.5).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_19, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_20, divine_marriage_command__coercion_visibility_reading, stakes_inflation(individual), 1910, 0.55).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_20, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_21, divine_marriage_command__coercion_visibility_reading, stakes_inflation(organizational), 1890, 0.8).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_21, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_22, divine_marriage_command__coercion_visibility_reading, stakes_inflation(organizational), 1910, 0.55).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_22, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_23, divine_marriage_command__coercion_visibility_reading, stakes_inflation(structural), 1890, 0.85).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_23, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_24, divine_marriage_command__coercion_visibility_reading, stakes_inflation(structural), 1910, 0.4).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_24, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_25, divine_marriage_command__coercion_visibility_reading, suppression(class), 1890, 0.55).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_25, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_26, divine_marriage_command__coercion_visibility_reading, suppression(class), 1910, 0.65).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_26, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_27, divine_marriage_command__coercion_visibility_reading, suppression(individual), 1890, 0.45).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_27, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_28, divine_marriage_command__coercion_visibility_reading, suppression(individual), 1910, 0.6).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_28, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_29, divine_marriage_command__coercion_visibility_reading, suppression(organizational), 1890, 0.35).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_29, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_30, divine_marriage_command__coercion_visibility_reading, suppression(organizational), 1910, 0.78).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_30, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_31, divine_marriage_command__coercion_visibility_reading, suppression(structural), 1890, 0.8).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_31, observed).
narrative_ontology:measurement(dmcmd_coercion_vis_grid_32, divine_marriage_command__coercion_visibility_reading, suppression(structural), 1910, 0.3).
narrative_ontology:measurement_basis(dmcmd_coercion_vis_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% Decomposition note for the divine_marriage_command family: the colloquial label 'the Manifesto' conflates three structurally distinct constraints. This story (coercion-visibility) authors epsilon for the standing arrangement — the ecclesiastically enforced post-Manifesto monogamy binding, 1890-1910 — assessed as an acknowledged coercion-response legitimated by institutional survival necessity. The continuationist sibling authors the same historical surface as a prudential suspension of a still-valid command (authorization resumable, practitioners remain inside the boundary, different victim set). The substitutionist sibling authors it as superseding revelation (legitimacy wholly internal, no coercion-exposure). Each file carries its own epsilon, beneficiaries, and victims; the family is linked through network.affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
