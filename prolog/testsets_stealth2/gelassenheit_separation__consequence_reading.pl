% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation — Consequence Reading: Practice-Preserving Technology Adjudication
 *   domain: religious/technological/commitment-systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the gelassenheit_separation
 *   kernel: the consequence reading, under which separation means preserving
 *   community practices and every technology is adjudicated by what it does
 *   to visiting, mutual aid, and geographic rootedness. A telephone is
 *   refused in the kitchen and granted in the barn shanty; a tractor is
 *   granted for belt power at the saw rig and refused for field traction; a
 *   website is refused at the shop desk and delegated to an English firm down
 *   the road. The test is consequential and practice-indexed, which is why
 *   this reading carries the lowest epsilon of the three sibling readings.
 *   Per the epsilon-invariance principle, the colloquial label 'Amish
 *   separation from technology' decomposes into three structurally distinct
 *   constraints: this one (test = effect on practices), the artifact reading
 *   (test = resemblance to worldly artifacts regardless of function), and the
 *   principle reading (test = functional isolation from worldly systems).
 *   Each is authored as its own story with its own epsilon, beneficiaries,
 *   and classification; they are linked via network.affects_constraints. The
 *   claim and the metrics are independent authored facts: the constraint is
 *   CLAIMED as rope, and the metrics are authored as descriptively true — low
 *   extraction, moderate suppression reflecting the shunning background and
 *   the cost of exit, low theater — without tuning either to the other or to
 *   a predicted engine output.
 *
 * KEY AGENTS:
 *   - district_bishops_and_ministers: agenda-setting seat (organized/identity_locked) — adjudicates each proposed device by its practice effects, lives under the same rules it administers, collects no salary
 *   - baptized_church_members: primary beneficiary body (moderate/identity_locked) — receives the protected visiting-and-aid fabric, pays at the margin of convenience
 *   - elderly_and_infirm_members: concentrated beneficiary (moderate/identity_locked) — the standing test case every relaxation is argued against
 *   - young_adults_pre_baptism: principal payer seat (moderate/constrained) — bound by rules they cannot vote on until baptism; rumspringa is their partial exit
 *   - member_business_owners: payer-beneficiary hybrid (moderate/constrained) — absorb efficiency costs, recover much through community trust and labor pooling
 *   - english_service_intermediaries: external beneficiary (moderate/mobile) — drivers, web firms, accountants whose livelihoods the restriction economy generates
 *   - departed_former_members: excluded voice (moderate/mobile) — know the costs from inside, never summoned to council
 *   - scholars_of_anabaptist_life: analytical observer (analytical/analytical) — supply the outside record of how the rules actually operate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.2).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.45).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation — Consequence Reading: Practice-Preserving Technology Adjudication").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/technological/commitment-systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, baptized_church_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, elderly_and_infirm_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, english_service_intermediaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, member_business_owners).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, young_adults_pre_baptism).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, member_business_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected ministers and bishops who chair the twice-yearly Ordnung council where proposed devices are taken up one by one. For each — a cellphone, a milking pipeline, a gas grill — they ask what it will do to Sunday visiting, to the barn-raising and harvest rosters, and to whether families stay on the land. They live under the same household rules they administer, take no salary, farm like their neighbors, and can impose confession or, after repeated refusal, shunning. Their authority rests on having been chosen from among the baptized and on keeping the district's practices visibly intact.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, district_bishops_and_ministers, agenda_setter,
    organized, generational, identity_locked, regional).

% Adult members who vowed at baptism to uphold the district's rules. They receive the protected goods directly: a web of near-daily visits, guaranteed help at barn raisings, weddings, funerals and hospital vigils, and children who grow up within walking distance. The price is paid at the margin of convenience — no phone in the house, no car in the shed, no internet line to the desk. Voice runs through the council and through petition; walking away means losing parents, siblings, inheritance expectations and, in their own accounting, their standing before God.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, baptized_church_members, beneficiary,
    moderate, generational, identity_locked, local).

% The oldest generation, for whom the arrangement is least abstract: the daily check-in visit, the ride roster, the sitting-up shifts after surgery, grandchildren nearby rather than a plane flight away. They hold moral weight in council deliberations — every proposed relaxation is argued against the test case of what happens to the shut-in when visiting thins — but they depend on others for nearly everything material.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, elderly_and_infirm_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Teenagers and unbaptized young adults who live inside rules they had no part in making. During rumspringa they may sample the outside world — a car, a phone, city wages — and return or leave without formal penalty, though family strain is real either way. They cannot vote in council until baptism, so their objections reach the agenda only filtered through parents and ministers. Roughly one in ten walks away for good; the rest accept a vow that retroactively authorizes the rules they grew up under.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, young_adults_pre_baptism, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, young_adults_pre_baptism, excluded).

% Woodshop, metalwork, quilt and construction operators who employ English workers and ship nationwide. The rules cost them: no internet-connected office computer, a phone shanty at the lane's edge instead of a desk extension, hired drivers for deliveries beyond buggy range. They recover much of it through the community's reputation for honest dealing and its deep bench of reliable labor, and they petition the council constantly — successfully more often than not — for business-use exceptions judged by the same practice-effects test.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, member_business_owners, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, member_business_owners, beneficiary).

% Non-Amish drivers, freight haulers, web designers and accountants whose livelihoods exist because the rules route around certain tasks rather than performing them in-house. A driver may make his living almost entirely off one settlement's need to reach job sites and hospitals; a web firm manages a dozen Amish shops' online storefronts that the shops themselves may not touch. They are paid market rates for real services and have no say in the rules that generate their customer base.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, english_service_intermediaries, beneficiary,
    moderate, biographical, mobile, regional).

% People who left — during rumspringa or after years inside — and now live as English neighbors, sometimes within sight of the old district. They hold the most detailed knowledge of what the restrictions cost from the inside and the most credible account of what exit actually takes, and they are never summoned to council. A few speak publicly; most are simply gone, their absence itself the community's standing evidence that the door is real.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, departed_former_members, excluded,
    moderate, biographical, mobile, national).

% Ethnographers and sociologists who have documented the technology councils, the phone-shanty compromises and the belt-power tractors for decades. They supply the outside record of how the rules actually operate — which exceptions get granted, what the departure rates are, how the criteria have shifted — and their accounts are read, warily, by both defenders and critics of the arrangement.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, scholars_of_anabaptist_life, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: technology adoption under strong neighborhood externalities. Any one family's home telephone, automobile or internet connection changes the incentive facing every neighbor — calls substitute for visits, cars widen the marriage and shopping radius, screens compete with evening company — and no individual family can preserve the visiting-and-aid fabric alone. The council mechanism lets the district adopt a device selectively (shared shanty phone, belt-power tractor, delegated website) while holding the practice-set constant, and lets it revisit each judgment as consequences become visible.
% TRANSFER_FUNCTION: Moves convenience and connectivity from member households — disproportionately from the young and from business operators — into preserved visit frequency, aid availability and land continuity that accrue to the whole body, heaviest to the old and infirm. A second, smaller stream moves cash outward: hauling fees, web-design retainers and accountancy bills paid to English intermediaries who perform what members may not.
% ABSENT_VOICES: Three seats are missing from the council room. Unbaptized youth live under rules they cannot vote on until they commit. Women carry much of the visiting-and-care workload the rules protect and are formally represented only through husbands and male relatives — the council is a men's table. And the departed, who know best what the restrictions cost from inside, are never asked. The surrounding English society, whose alternatives define the choice window, speaks only through market prices and the presence of its technologies.
% DISAPPEARANCE_RATIONALE: If the council mechanism and its rules vanished overnight, adoption would cascade household by household — phones migrate from shanty to kitchen, buggies to second cars, shop floors to connected offices — and within a decade the visit-and-aid economy would thin into appointment-and-market substitutes. Land near growing towns would sell to commuters, schools would consolidate outward, and the settlements would drift toward assimilated Mennonite or plain-adjacent forms. Nothing in the surrounding economy holds the practices up; they persist only because the rules hold the incentives in place.
% FOUNDING_PROBLEM: How a committed minority can keep the face-to-face practices — visiting, mutual aid, staying on the land — that constitute its religious life while embedded in an industrializing society whose every useful device tends to dissolve exactly those practices. The early-twentieth-century telephone crisis made it acute: a device cheap enough for every farmhouse and potent enough to replace the doorstep visit.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: a century of ethnographic record (Hostetler, Kraybill, Johnson-Weiner and successors) documents the problem as ongoing rather than solved, and the steady stream of rumspringa negotiations, council minutes and affiliation switches attests the tension is real. Even hostile witnesses — critics of shunning and of educational limits — attest that the community faces a genuine trade-off between device adoption and practice survival rather than a cover for private gain; no seat captures proceeds from the rules.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.20) because the rules take preference and convenience at the margin rather than wealth or labor product, are calibrated device-by-device to affirmed ends, and are revisable through petition; the gentle decline across the series reflects the reading's own method working — barn phones, belt-power tractors, and delegated digital work each lowered the cost of compliance without surrendering the protected practices. Theater is low (0.12) and falling because under this reading a device's appearance is incidental; marker maintenance is the artifact reading's constitutive activity, not this one's. Accessibility collapse is low-moderate (0.30) because alternatives persist in every direction: forty-plus affiliations spanning strict to lenient, Mennonite cousin communities, intra-Ordnung workarounds, and a tolerated exit door. Resistance is moderate (0.35): youth negotiation, entrepreneur petitions, and recurring affiliation switching, historically including schisms over the telephone and the car. Suppression (0.45) is the highest metric and deliberately so: the shunning threat is severe even though rarely invoked, exit costs are identity-scale, and part of the compliance is likely internalized rather than structural — the omega variable carries that ambiguity. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: device proliferation through the twentieth century forced a build-out of adjudication machinery (ministers' meetings, conference committees, standing technology review), which plateaued once the workaround repertoire standardized. All three series run on one shared six-point grid (1910, 1935, 1960, 1985, 2005, 2020) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the bishop's seat the arrangement is a discipline it also obeys: no salary, no exemption, the same shanty phone at the lane's edge — a coordination mechanism it stewards rather than profits from. From the youth seat the same rules bind before consent and voice arrives only after the vow; that seat should compute a harsher type. From the departed member's seat the structure is whatever it cost them to leave, with no compensating access to the good. From the intermediary's seat it is a subsidy. The engine computes this divergence from the structural data; the authored rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (baptized members, the elderly, English intermediaries) derive low directionality — the arrangement subsidizes them. The payer seats (youth, business owners) derive elevated directionality from their payer roles but sit mid-range rather than at the target end, because their situations embed offsetting returns: the youth seat includes a tolerated exit window, the business-owner seat includes the trust premium and labor pool the rules help sustain. No victims array is declared because no seat bears costs on unfair terms by the structure's own lights — the costs are the accepted price of the good, contestable through voice and exit, which is precisely what separates this from an extraction target. The bishop seat is undeclared and falls to fallback, moderated by its own complete subjection to the rules it administers. Receipt surface: gains accrue diffusely to the member body as preserved visiting, aid availability, and land continuity; the intermediaries earn market-rate fees for real services rendered, not captured extraction — hence gain_flow is authored as an affirmative 'diffuse'. fixing_cost is omitted: for a healthy coordination arrangement the fix/removal question presupposes a pathology the story does not establish.
 *
 * MANDATROPHY ANALYSIS:
 *   The misclassification risks run in both directions. Outside observers pattern-match shunning and rule density to pure extraction; traditionalist apologetics pattern-match longevity and uniformity to natural law. The rope verdict holds the middle because the structural facts support it: a genuine collective-action problem (technology adoption under neighborhood externalities that no family can resist alone), participants who are net beneficiaries by revealed preference under a real exit option, and enforcement that is real but light, consensual, and aimed at the practice-set rather than at any rent. The omegas carry the two live doubts that would move the computation — consent validity and gendered burden — so the classification is falsifiable rather than settled. The founding problem remains live (every new device re-presents it), so no mandatrophy is declared: the arrangement has not outlived its function, and the mandate and the function still coincide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the gelassenheit_separation kernel. What structurally changes if a sibling reading governs instead?',
    'Compare the three reading-stories'' classifications: artifact_reading should score higher theater (marker maintenance is constitutive of it) and higher suppression (function-irrelevant refusals invite evasion); principle_reading shifts the cost-bearing set toward actors entangled through business and infrastructure systems.',
    'If artifact_reading governed, refusals would decouple from function, raising theater_ratio and the arbitrariness costs borne by business owners; if principle_reading governed, the test referent moves from community practices to systemic entanglement, changing which devices pass and whose costs count.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the separation kernel; the disagreement is located in the admissibility test a device must pass.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (shunning threat, exit costs, closed information environment) or internalized (members who no longer experience the rules as constraining because desire itself was formed inside them)?',
    'Post-exit trajectory of leavers: if former members report durable norm-holdover (guilt, reflexive compliance) long after exit, a substantial internalized component is established.',
    'If largely internalized, effective suppression exceeds the structural measure and the revealed preference of stayers is weaker evidence of net benefit; if largely structural, relaxing enforcement would release preferences the scalar currently cannot see.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized split of community suppression.').

omega_variable(
    baptismal_consent_validity,
    'Does adult baptism constitute valid consent to a lifetime of technology restrictions when the chooser was raised inside the rules with no experienced alternative except a bounded rumspringa?',
    'Compare preference stability between stayers who sampled the outside world extensively during rumspringa and those who sampled minimally; test whether departure rates track exposure breadth.',
    'If consent is substantially manufactured by socialization closure, the costs imposed on stayers stop counting as waived and effective extraction rises, trending the computed type toward tangled_rope; if rumspringa exposure grounds real consent, the rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baptismal_consent_validity, conceptual, 'Whether the community''s consent structure validates its cost imposition.').

omega_variable(
    gendered_cost_distribution,
    'Do the rules distribute their costs by gender — women carrying much of the visiting-and-care workload the rules protect while holding no formal council voice?',
    'Time-use and workload studies inside settlements, plus analysis of which petitions reach the men''s council and through whom they arrive.',
    'If costs are systematically gendered alongside a voice asymmetry, extraction rises above the authored 0.20 and women members'' directionality moves toward the target end, pulling the computed type toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_cost_distribution, empirical, 'Gender asymmetry in burden versus voice under the practice-preservation rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 1910, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1910, gelassenheit_separation__consequence_reading, theater_ratio, 1910, 0.18).
narrative_ontology:measurement(gela_tr_t1935, gelassenheit_separation__consequence_reading, theater_ratio, 1935, 0.17).
narrative_ontology:measurement(gela_tr_t1960, gelassenheit_separation__consequence_reading, theater_ratio, 1960, 0.16).
narrative_ontology:measurement(gela_tr_t1985, gelassenheit_separation__consequence_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(gela_tr_t2005, gelassenheit_separation__consequence_reading, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(gela_tr_t2020, gelassenheit_separation__consequence_reading, theater_ratio, 2020, 0.12).

% Extraction over time
narrative_ontology:measurement(gela_be_t1910, gelassenheit_separation__consequence_reading, base_extractiveness, 1910, 0.26).
narrative_ontology:measurement(gela_be_t1935, gelassenheit_separation__consequence_reading, base_extractiveness, 1935, 0.25).
narrative_ontology:measurement(gela_be_t1960, gelassenheit_separation__consequence_reading, base_extractiveness, 1960, 0.23).
narrative_ontology:measurement(gela_be_t1985, gelassenheit_separation__consequence_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(gela_be_t2005, gelassenheit_separation__consequence_reading, base_extractiveness, 2005, 0.21).
narrative_ontology:measurement(gela_be_t2020, gelassenheit_separation__consequence_reading, base_extractiveness, 2020, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1910, gelassenheit_separation__consequence_reading, suppression_requirement, 1910, 0.38).
narrative_ontology:measurement(gela_su_t1935, gelassenheit_separation__consequence_reading, suppression_requirement, 1935, 0.4).
narrative_ontology:measurement(gela_su_t1960, gelassenheit_separation__consequence_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement(gela_su_t1985, gelassenheit_separation__consequence_reading, suppression_requirement, 1985, 0.44).
narrative_ontology:measurement(gela_su_t2005, gelassenheit_separation__consequence_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(gela_su_t2020, gelassenheit_separation__consequence_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, attachment_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'Amish separation from technology' is one colloquial label covering three structurally distinct claims. This story authors the consequence reading (test = effect on visiting, mutual aid, rootedness; epsilon ~0.20, theater low). The artifact reading (test = resemblance to worldly artifacts regardless of function) carries higher theater because marker maintenance is constitutive of it, and the principle reading (test = functional isolation from worldly systems) relocates the contested costs to business and infrastructure entanglement. Historical structure: the artifact reading preceded this one and still supplies the visible markers this reading retains incidentally; the principle reading shares this reading's functionalism but aims it at system independence rather than practice survival. Each family file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
