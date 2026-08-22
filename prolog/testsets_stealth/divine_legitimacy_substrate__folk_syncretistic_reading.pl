% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Folk Syncretistic Reading: Household-Village Ritual Substrate of Divine Legitimacy
 *   domain: religious/political-economy/ancient-history
 *
 * SUMMARY:
 *   In the folk syncretistic reading of the divine-legitimacy kernel, divine
 *   legitimacy flows through household and village ritual practice: domestic
 *   shrines to Bes, Taweret, and Hathor, ancestor veneration, amulet use,
 *   village festivals, and pragmatic incorporation of whichever deity proves
 *   locally efficacious. This is one of three readings of the same kernel.
 *   The amun_polytheistic_reading locates legitimacy in priestly
 *   interpretation of official multi-deity cosmology; the
 *   atenist_monotheistic_reading locates it solely in pharaonic revelation of
 *   Aten, with all other gods declared false. This story instantiates only
 *   the folk reading: the arrangement under contest is the household-village
 *   ritual substrate itself, assessed by its own lights, with epsilon
 *   authored for that standing arrangement and not for any arrangement a
 *   sibling reading would install. Its authority is diffuse: no seat
 *   administers it, thousands of households reproduce it, and pharaoh and
 *   priesthood stand outside it as distant elites whose mediation reaches
 *   villages only episodically. Claim and metrics are authored independently:
 *   the constraint is claimed as rope, and the metrics describe
 *   low-extraction, low-suppression operation with a slow extraction creep as
 *   the specialist stratum and temple festival economy learned to charge for
 *   access to what households already practiced.
 *
 * KEY AGENTS:
 *   - - village_households: participant-beneficiary and cost-bearer (moderate/constrained) — reproduce the practice, receive its protections, bear its offering and labor costs
 *   - - domestic_cult_women: primary labor-bearing seat (moderate/constrained) — perform the daily and crisis ritual on which continuity rests, with protections accruing household-wide
 *   - - local_ritual_specialists: service-stratum beneficiary (moderate/mobile) — collect fees and status for protection, healing, and life-cycle rites no distant institution delivers
 *   - - amun_priesthood: excluded elite (institutional/constrained) — custodian of official cosmology, bypassed by household practice while harvesting its festival traffic
 *   - - pharaonic_state: excluded elite (institutional/constrained) — claims divine sonship, cannot redirect the substrate by decree, prices taxation around it
 *   - - historians_of_religion: analytical observer (analytical/analytical) — reconstructs the substrate from archaeological remains the official record ignores
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.28).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Folk Syncretistic Reading: Household-Village Ritual Substrate of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political-economy/ancient-history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '04ce6862-082c-4126-9f37-cd5612a04921').
narrative_ontology:cs_kernel_codification('04ce6862-082c-4126-9f37-cd5612a04921', distributed).
narrative_ontology:cs_authority_grounding('04ce6862-082c-4126-9f37-cd5612a04921', practice).
narrative_ontology:cs_interpretation_layer_present('04ce6862-082c-4126-9f37-cd5612a04921').
narrative_ontology:cs_reading_relation('04ce6862-082c-4126-9f37-cd5612a04921', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('04ce6862-082c-4126-9f37-cd5612a04921', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_axiom('04ce6862-082c-4126-9f37-cd5612a04921', foundational, efficacy_validates_practice).
narrative_ontology:cs_axiom_status(efficacy_validates_practice, holdable).
narrative_ontology:cs_axiom_grounding('04ce6862-082c-4126-9f37-cd5612a04921', efficacy_validates_practice, empirically_contingent).
narrative_ontology:cs_axiom('04ce6862-082c-4126-9f37-cd5612a04921', foundational, plural_accessibility_of_divine_power).
narrative_ontology:cs_axiom_status(plural_accessibility_of_divine_power, holdable).
narrative_ontology:cs_axiom_grounding('04ce6862-082c-4126-9f37-cd5612a04921', plural_accessibility_of_divine_power, theological).
narrative_ontology:cs_reference_frame('04ce6862-082c-4126-9f37-cd5612a04921', plural_local_efficacy_norm).
narrative_ontology:cs_drift_state('04ce6862-082c-4126-9f37-cd5612a04921', late_new_kingdom_oracle_economy, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('04ce6862-082c-4126-9f37-cd5612a04921', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_households).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, domestic_cult_women).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, village_households).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, domestic_cult_women).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, decentralized_plural_resilience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain a domestic shrine, keep ancestor busts and offering tables, commission amulets for pregnancy and childhood, and provision village festivals. Protection, healing, fertile herds, and safe passage to the afterlife are understood to flow from regular observance. The costs recur: bread, beer, flowers, figurines, festival levies, and hours of preparation. Leaving the practice means forfeiting the reciprocity that runs through festival participation and neighborly obligation; a household that stops giving finds funerals and harvests lonelier.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_households, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, village_households, payer).

% Carry the daily and crisis ritual labor: tending the household shrine, performing protective rites over pregnancy, birth, and infancy, mourning the dead, and feeding ancestors on the calendar. The continuity of household observance rests on this unpaid work, while its protections accrue to everyone under the roof. Skilled women convert the same competence into standing as wise women and midwives, paid in grain, cloth, or deference.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, domestic_cult_women, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, domestic_cult_women, beneficiary).

% Part-time lector priests, wise women, amulet makers, and festival officiants sell what no distant institution delivers at village scale: spells copied to order, charms against scorpion sting, divination before a journey, rites at the tomb. Income arrives as fees and shares of offerings. Entry barriers are low, so competition among practitioners caps what any one can charge, and a specialist's reputation travels poorly beyond the home villages.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists, beneficiary,
    moderate, biographical, mobile, local).

% Administers the great temple estates and the official cosmology with Amun-Ra at its head. Its mediation reaches villages episodically, through festival processions, oracle days, and votive commerce at temple gates. Household practice runs whether or not the temples show up, and the priesthood's claim that legitimacy requires its interpretation finds little purchase at the domestic shrine. It draws festival crowds and votive income from the substrate while remaining outside its administration.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, amun_priesthood, excluded,
    institutional, generational, constrained, national).

% Rules through the claim of divine sonship and accommodates village religion through festival patronage and tax schedules priced around it. The one attempt to redirect legitimacy by decree, the Atenist episode, collapsed against household observance and was reversed; since then the court treats the substrate as an immovable floor, honored in procession and ignored in doctrine.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_state, excluded,
    institutional, civilizational, constrained, national).

% Reconstruct household and village religion from what elite texts never bothered to record: domestic shrine architecture at Deir el-Medina, amulet hoards, votive deposits, graffiti, and settlement archaeology. Their seat exists because the official record systematically under-describes the arrangement this story is about.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, local_ritual_specialists).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__folk_syncretistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns village life on a shared ritual calendar of festivals, processions, and life-cycle rites, so that transitions are marked once, communally, instead of improvised per household; pools protective knowledge about which rite, which deity, which practitioner answers which misfortune; and converts private crisis into shared, actionable observance.
% TRANSFER_FUNCTION: Moves bread, beer, flowers, figurines, cloth, and small valuables from household stores to deity cults, specialist fees, and festival funds; moves labor, preparation, procession, and mourning, from household members, disproportionately women, into the ritual calendar. Moving back: protection, healing, divination, legitimated life transitions, and standing in the village's reciprocal economy.
% ABSENT_VOICES: The Amun priesthood and the royal house would object that legitimacy cannot flow outside authorized mediation; they are present in the world but absent from this conversation, which is conducted at hearths and crossroads without them. Also missing are villagers who found the costs burdensome or doubted the efficacy of what they paid for; illiteracy and the fragility of everyday testimony mean skeptical voices survive only as rare jokes and tomb complaints.
% DISAPPEARANCE_RATIONALE: Life-cycle transitions would lose their legitimated communal form; the specialist stratum would lose its livelihood; festival reciprocity, a major redistribution mechanism in village economies, would lapse; and both elite readings would find nothing beneath them to draw on, since the priesthood's festival traffic and the crown's ceremonial acquiescence presuppose households that already practice.
% FOUNDING_PROBLEM: Existential precarity managed without waiting on distant institutions: childbirth and infant mortality, crop failure, disease, scorpions, and a dangerous afterlife passage, plus the practical need to mark births, marriages, and deaths in a way the village recognizes.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the archaeological record of domestic shrines, amulet hoards, and votive volume at non-elite sites attests practice intensity independent of any participant's self-report; demographic evidence attests the underlying mortality the practice addresses; and the Atenist interlude supplies a controlled test, since household observance continued when elite channels closed, which only makes sense if the founding problem was live independently of elite provision. Elite texts complaining of popular superstition further attest the substrate's independence from the elite's own framing.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.28 at interval end) because the referent arrangement is broadly reciprocal: offerings buy protection, fees buy services, and the specialist stratum's take is capped by low entry barriers and local competition. The creep visible in the measurement series reflects the late-interval oracle and festival economy, which added skimmable surface without changing the substrate's core. Suppression is low (0.18) because there is no enforcement machinery: conformity runs on village reciprocity plus internalized caution about demons and the angry dead, and rival options are absorbed rather than suppressed, which is what pragmatic incorporation means. Theater is low (0.18): for participants the performance is the function, and formulaic drift is modest. Accessibility_collapse is low (0.22) because understanding this arrangement does not collapse alternatives; syncretism keeps rival deities and practices permanently live. Resistance is low-moderate (0.28): episodic elite hostility, sharpest during the Atenist interlude, plus scattered internal skepticism, against broad acceptance. The three series share one grid at {0, 20, 40, 60, 80, 100}. Suppression_requirement is tracked deliberately because this story traces enforcement-capacity dynamics: the requirement stays near zero throughout, dipping lowest at T40 when post-Amarna vindication of the traditional cults made adherence maximally voluntary, then rising slightly as commercialized festival obligations needed more social policing. Receipt surface: what extraction exists accrues chiefly to the specialist stratum, hence gain_flow names local_ritual_specialists. Fixing cost is prohibitive: no seat can fix or remove the arrangement, and the single historical removal attempt consumed its sponsor's dynasty and was reversed within a generation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from inside. Households sit near symmetric: provision received against costs borne. Women within those households carry a labor-cost asymmetry inside a net-beneficial household account, so their seat reads harsher than the household aggregate. Specialists see a livelihood. The excluded elite seats read the same arrangement as threat or noise: from the priesthood's position, legitimacy flowing outside authorized mediation undermines its mediation monopoly; from the throne's position, the substrate is an immovable floor that defeated the one attempt to replace it. Same-power divergence is sharp here: specialists and domestic-cult women hold identical nominal power (moderate) but opposite cost-benefit positions, differentiated by exit options (mobile versus constrained) and role, not by rank. The engine computes these divergences from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries are village_households and local_ritual_specialists, deriving low directionality for both. No victims are declared because costs are broadly reciprocal: no seat sits at the full-target pole. The nearest-to-target seat is domestic_cult_women, whose payer-first dual role derives a directionality moderately above symmetric, reflecting labor borne against protections shared. Dual-role households derive near symmetric. The excluded elite seats fall outside the beneficiary/victim derivation; their indirect positions (the priesthood harvesting festival traffic, the state taxing around the substrate) are noted here rather than overridden, because overrides are keyed by power atom and an institutional-level override would also strike the moderate-power specialists, distorting their correctly derived beneficiary directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards in both directions. Against the elite-source misreading: official texts dismiss household practice as superstition, which would misread a functional coordination substrate as mere error or extraction; the rope claim keeps the genuine coordination function on the books. Against the romantic over-read: the specialist stratum's fees and the late oracle economy are real, if modest, extraction layered onto coordination, and the rising base_extractiveness series records that creep without letting it redefine the whole arrangement. The founding problem, misfortune-management and transition-marking without elite mediation, remains live across the entire interval; there is no sunset clause and no atrophied mandate, so mandatrophy is not resolved. The mismatch consumer should find status=live combined with verdict=world_rearranges coherent: no zombie flag is expected, and a computed divergence here would itself be signal worth investigating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel divine_legitimacy_substrate (reading: folk_syncretistic_reading). Which structural facts would change under the sibling readings?',
    'Cross-reading comparison of the three family stories: recompute beneficiary and victim sets and epsilon under amun_polytheistic_reading, which recenters benefit on the priesthood and raises extraction for excluded villages, and under atenist_monotheistic_reading, which converts traditional practitioners into targets.',
    'Under the Atenist sibling the victim set expands to nearly every seated agent and suppression rises sharply; under the Amun sibling the specialist stratum becomes a captured intermediary. The folk reading''s low epsilon is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one of three readings of the divine-legitimacy kernel; sibling adoption changes beneficiary structure and epsilon.').

omega_variable(
    diffuse_beneficiary_attribution,
    'In an arrangement administered by no one, who captures net benefit: participating households collectively, the specialist stratum, or elite institutions harvesting the substrate''s festival traffic?',
    'Household-level accounting of ritual expenditure against insurance value received; stratification analysis of specialist income; temple estate accounts for festival-era votive flows.',
    'If specialists or temples capture systematically, the arrangement drifts toward hybrid coordination-extraction and the rope claim weakens; if households net-benefit, rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_beneficiary_attribution, empirical, 'Beneficiary structure is genuinely unclear for a constraint with no administrator; attribution decides the rope-versus-hybrid question.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the conformity pressure sustaining observance structural (village reciprocity, social sanction) or internalized (fear of demons and the angry dead that travels with the practitioner)?',
    'Post-disruption trajectory: communities relocated or newly founded, such as royal workmen''s villages, reveal whether observance norms reconstitute without neighbor enforcement or whether anxiety-driven practice persists in isolation.',
    'A dominant internalized share raises effective suppression above the structural measure and strengthens identity-lock dynamics for the household seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in a low-enforcement interpersonal-communal arrangement.').

omega_variable(
    enforcement_free_persistence,
    'Does the substrate persist because it solves problems participants recognize (voluntary reproduction) or because inherited obligation makes neglect unthinkable?',
    'Compare the speed and completeness of ritual reconstruction in freshly founded settlements against inherited villages; track observance among households migrating to cosmopolitan centers away from kin enforcement.',
    'Voluntary reconstruction confirms the rope reading; obligation-only maintenance would date a drift toward inertial persistence and revise the classification downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_free_persistence, empirical, 'Whether persistence reflects solved collective-action problems or unbreakable inherited obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dls_folk_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dls_folk_tr_t20, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(dls_folk_tr_t40, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(dls_folk_tr_t60, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(dls_folk_tr_t80, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 80, 0.17).
narrative_ontology:measurement(dls_folk_tr_t100, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(dls_folk_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dls_folk_be_t20, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(dls_folk_be_t40, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(dls_folk_be_t60, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement(dls_folk_be_t80, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement(dls_folk_be_t100, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(dls_folk_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(dls_folk_su_t20, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(dls_folk_su_t40, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(dls_folk_su_t60, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 60, 0.13).
narrative_ontology:measurement(dls_folk_su_t80, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement(dls_folk_su_t100, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: divine_legitimacy_substrate decomposes into three readings, amun_polytheistic_reading (priestly mediation), atenist_monotheistic_reading (exclusive royal revelation), and folk_syncretistic_reading (this file, household-village practice). The decomposition follows the epsilon-invariance principle: the colloquial label 'ancient Egyptian religion' conflates three structurally distinct legitimacy arrangements with different beneficiary sets, enforcement profiles, and epsilon values, so each gets its own story. The folk substrate is upstream of both siblings: elite readings draw festival traffic, votive income, and ceremonial acquiescence from it, and the Atenist sibling's failure against it is the historical demonstration of its resilience. Edges here run from this reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
