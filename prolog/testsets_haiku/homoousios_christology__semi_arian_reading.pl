% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Semi-Arian Christology: Homoiousios (Similar Substance) Doctrine
 *   domain: ecclesiastical_politics/historical_theology
 *
 * SUMMARY:
 *   Between the Council of Nicaea (325) and the Council of Constantinople
 *   (381), the homoiousios (similar substance) christology emerged as a
 *   compromise position in the bitter dispute over Christ's ontological
 *   relation to God the Father. The Pro-Nicene faction insisted on homoousios
 *   (identical substance); the Arian faction held Christ as created and
 *   subordinate. The Semi-Arian reading, articulated primarily by Basil of
 *   Ancyra and other moderate bishops, proposed that Christ shares the
 *   Father's substance *in kind* but not *in number* — a middle ground
 *   designed to prevent permanent schism and allow imperial ecclesiastical
 *   unity. Historically, this compromise absorbed competing factions'
 *   concerns temporarily but ultimately failed: the Pro-Nicene position
 *   triumphed at Constantinople (381), and the Semi-Arian formula was
 *   absorbed into Pro-Nicene orthodoxy, stripped of its subordinationist
 *   readings. The measurement series tracks this arc: extractiveness and
 *   theater rise sharply as the compromise's enforcement cost grows
 *   (345-355), then decline as Pro-Nicene theology consolidates authority and
 *   renders the Semi-Arian formula obsolete.
 *
 * KEY AGENTS:
 *   - Moderate episcopal faction (Basil of Ancyra, George of Laodicea): agenda-setters formulating and defending the homoiousios doctrine
 *   - Strict Nicene advocates (Athanasius, core Pro-Nicene theologians): reject the compromise as insufficient, bear cost of doctrinal confusion
 *   - Arian sympathizers (eastern bishops, subordinationist sympathizers): exploit the compromise for doctrinal cover but lose clarity
 *   - Imperial authority (Constantius II especially): benefits from negotiated settlement and reduced schism risk
 *   - Parish clergy: excluded, forced to catechize confusing distinctions
 *   - Theological interpreters (post-4th century historians): observe and analyze the compromise's failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.45).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Semi-Arian Christology: Homoiousios (Similar Substance) Doctrine").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "ecclesiastical_politics/historical_theology").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '3f033435-8831-46a6-a017-4f882c299cad').
narrative_ontology:cs_kernel_codification('3f033435-8831-46a6-a017-4f882c299cad', fixed_text).
narrative_ontology:cs_authority_grounding('3f033435-8831-46a6-a017-4f882c299cad', extraction).
narrative_ontology:cs_interpretation_layer_present('3f033435-8831-46a6-a017-4f882c299cad').
narrative_ontology:cs_reading_relation('3f033435-8831-46a6-a017-4f882c299cad', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('3f033435-8831-46a6-a017-4f882c299cad', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('3f033435-8831-46a6-a017-4f882c299cad', foundational, christ_similar_substance_not_identical).
narrative_ontology:cs_axiom_status(christ_similar_substance_not_identical, holdable).
narrative_ontology:cs_axiom_grounding('3f033435-8831-46a6-a017-4f882c299cad', christ_similar_substance_not_identical, deontological).
narrative_ontology:cs_axiom('3f033435-8831-46a6-a017-4f882c299cad', foundational, schism_prevention_via_negotiated_doctrinal_mean).
narrative_ontology:cs_axiom_status(schism_prevention_via_negotiated_doctrinal_mean, overridden).
narrative_ontology:cs_axiom_grounding('3f033435-8831-46a6-a017-4f882c299cad', schism_prevention_via_negotiated_doctrinal_mean, instrumental).
narrative_ontology:cs_reference_frame('3f033435-8831-46a6-a017-4f882c299cad', apostolic_substance_compromise).
narrative_ontology:cs_drift_state('3f033435-8831-46a6-a017-4f882c299cad', post_nicene_reaction_phase, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3f033435-8831-46a6-a017-4f882c299cad', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, moderate_episcopal_faction).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, schism_avoiders).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, strict_nicene_advocates).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, arian_sympathizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and theologians (Basil of Ancyra, George of Laodicea, and allied eastern bishops) who formulate the homoiousios doctrine and defend it as the via media. They author council creeds, write polemical treatises, and press the formula through imperial channels and episcopal networks. They claim to preserve both Nicene precision and apostolic tradition without rigid dogmatism. By 370-381, their power wanes as Pro-Nicene consolidation accelerates, but they maintain institutional positions and attempt to negotiate favorable terms for the absorption of their formula.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, moderate_episcopal_faction, agenda_setter,
    institutional, generational, constrained, continental).

% Athanasius, the Nicene core theologians (including later Gregory of Nyssa and Gregory of Nazianzus before their reconciliation with Semi-Arianism), and their episcopal allies. They read homoousios as the only doctrinally sufficient formula against Arianism and view homoiousios as a capitulation that preserves Arian subordinationism under new language. They are forced to accommodate the compromise or risk isolation; they pay the cost of doctrinal dilution and the need to police Semi-Arian interpretations of homoiousios. Their victory comes only at Constantinople (381) when Pro-Nicene terminology triumphs.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, strict_nicene_advocates, payer,
    powerful, generational, constrained, continental).

% Eastern bishops (Eusebius of Nicomedia's successors, subordinationist sympathizers in Syria and the Levant) who exploit homoiousios as doctrinal cover. They can affirm the formula while reading it as affirming Christ's subordination to the Father. They lose the clarity of pure Arianism but gain the legitimacy of imperial orthodoxy. After 381, they are suppressed as covert Arians; the Semi-Arian formula's absorption into Pro-Nicene orthodoxy strips their doctrinal shield.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, arian_sympathizers, payer,
    moderate, generational, constrained, continental).

% Constantius II (337-361) especially, and later emperors (Julian briefly, then Valens 364-378, then Theodosius). They convene councils (Antioch 341, Sirmium 358, Constantinople 381) to resolve doctrinal disputes and prevent schism. They benefit from the homoiousios compromise as a negotiated settlement that avoids permanent fracture. Constantius champions the formula; after his death and Julian's interregnum, later emperors shift toward Pro-Nicene support. The shift reflects political calculation: Pro-Nicene theology eventually appears as a stronger legitimizing force.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).

% Parish priests and deacons responsible for catechizing congregations and defending the creed. They are not party to the doctrinal negotiations; they receive creeds from their bishops and must teach them. Homoiousios is notoriously difficult to explain — the distinction between 'identical substance' and 'similar substance' is subtle and easily muddled in a sermon or catechetical session. They are trapped: they must teach what their bishop imposes, but the formula itself is ambiguous and difficult, making their pastoral role harder. They are excluded from the doctrinal decisions that burden them.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, parish_clergy, excluded,
    powerless, biographical, trapped, local).

% Syrian, Egyptian (Coptic), and other non-Greek-speaking churches that are subject to the decisions of the Greek-dominated councils but are not full participants in the negotiations. They receive the homoiousios formula from imperial decree and must conform or schism. Their absence from the doctrinal disputes means they have no voice in the compromise's terms. Later (4th-5th centuries), some of these churches reject the formula and maintain alternative christologies.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, non_greek_churches, excluded,
    powerless, generational, trapped, regional).

% Theologians, scholars, and later historians (Jerome, Augustine, Epiphanius of Salamis) who analyze the christological disputes and their resolution. They study how the homoiousios formula functioned as a temporary coordination mechanism, track its absorption into Pro-Nicene orthodoxy, and assess its theological adequacy. Their role is interpretive and retrospective; they observe the constraint's operation and its failure to prevent ultimate schism.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, theological_interpreters, observer,
    organized, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal formula (homoiousios) that allows bishops holding incompatible christologies to participate in a single church structure and affirm a shared creed without forcing either Pro-Nicene or Arian sympathizers to fully capitulate. The formula states that Christ shares the Father's substance 'in kind' (homoios) but not 'in number' (ouisia), allowing Pro-Nicene readers to interpret this as affirming Christ's full divinity while Arian readers can read it as affirming Christ's subordination within a divine hierarchy. The coordination solves the schism problem temporarily by providing ambiguous language both sides can ostensibly accept.
% TRANSFER_FUNCTION: Moves doctrinal authority from individual bishops and local theologians to the imperial-convened ecumenical council and the moderate episcopal faction. Bishops surrender the right to teach their own christological position without council approval in exchange for the promise of a negotiated settlement that avoids permanent schism. Imperial authority gains the legitimacy of theological consensus (even if false consensus). Strict Nicene advocates lose the unambiguous victory they sought at Nicaea; they must police Semi-Arian interpretations of homoiousios and suppress heterodox readings. Arian sympathizers lose the clarity of subordinationism but gain temporary institutional legitimacy.
% ABSENT_VOICES: Non-Greek-speaking churches (Syrian, Egyptian Coptic), lay theologians, non-episcopal clergy, and women are entirely absent from the doctrinal negotiations. Parish clergy who must catechize the formula are not consulted in its formulation. These absent parties would object to the imposition of a compromise they did not author. Some later churches explicitly reject homoiousios and maintain alternative readings. The decision is made by Greek-speaking episcopal elites and imperial authority, narrowing the legitimacy base of the formula.
% DISAPPEARANCE_RATIONALE: If the homoiousios compromise had never been formulated, the Nicene-Arian split would have hardened earlier and more severely. The church would have organized into smaller, more coherent doctrinal factions (Pro-Nicene strongholds in Egypt and western regions, semi-Arian and Arian strongholds in the east and north) rather than maintaining a fragile unified structure under an ambiguous formula. The compromise delayed but did not prevent the shift toward Pro-Nicene dominance; without it, that shift would have occurred by 355-360, eliminating the intermediate Semi-Arian institutional space. Imperial church policy would have bifurcated earlier, and the religious landscape would have fragmented along clearer doctrinal lines.
% FOUNDING_PROBLEM: The Council of Nicaea (325) condemned Arianism and affirmed homoousios (Christ is of identical substance with the Father) but left the church fractured. The strict Nicene faction insisted on the formula's absolute precision; Pro-Nicene bishops viewed any softening as a reintroduction of subordinationism. By the 330s-340s, Arian bishops (Eusebius of Nicomedia and allies) regrouped and challenged Nicene supremacy. Imperial politics shifted: Constantine favored the Nicenes, but his successor Constantius II (337-361) was more open to Arian sympathizers. The risk of permanent schism — eastern and western churches teaching contradictory christologies, imperial authority divided — became acute. The homoiousios formula was formulated to find a middle ground that would allow the church to remain institutionally unified under a shared (if ambiguous) creed.
% FOUNDING_PROBLEM_CORROBORATION: Constantius II and moderate bishops (Basil of Ancyra, George of Laodicea) attest that the founding problem (schism risk, post-Nicene doctrinal collapse) is live and urgent, and that homoiousios solves it by allowing negotiated coexistence. Athanasius and Pro-Nicene bishops attest the problem is the Arian heresy itself, not doctrinal imprecision; they argue homoiousios exacerbates the problem by providing cover for subordinationism. Later historians (Eusebius of Caesarea, Epiphanius, Athanasius in his polemics) and post-381 theological analysis attest that the compromise succeeded temporarily in preventing the most acute schism (345-370) but ultimately failed: the problem persisted, deepened, and was resolved only by Pro-Nicene victory at Constantinople (381) and subsequent suppression of Semi-Arian and Arian readings. The compromise itself became a problem — a placeholder that satisfied no one permanently.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.28) immediately after Nicaea because the compromise has not yet been formulated — it rises sharply through the 340s-350s (peaking at 0.51 in 355) as the Semi-Arian formula gains institutional traction and both Pro-Nicene and Arian factions experience the cost of ambiguity and enforced doctrinal conformity. The theater ratio rises even more steeply (0.22→0.49, 325-355) because the compromise increasingly becomes performative: both Pro-Nicene and Arian bishops can affirm homoiousios while interpreting it according to their own theology, turning the formula into a theatrical consensus that masks continued disagreement. Suppression requirement tracks enforcement cost: bishops must police each other's interpretations and suppress heterodox readings of homoiousios, requiring active councils and imperial intervention. By 381, both extractiveness and theater decline as the compromise is absorbed and Pro-Nicene theology becomes dominant — the Semi-Arian formula loses functional power and becomes a historical artifact. All three metrics share one time grid (325, 335, 345, 355, 370, 381) so temporal analysis is alignment-clean.
 *
 * PERSPECTIVAL GAP:
 *   The moderate episcopal faction (agenda-setters) and the imperial authority experience the homoiousios constraint as genuine coordination — a negotiated settlement that prevents schism and preserves church unity. The strict Nicene advocates experience it as enforced dilution of their theological victory and as a vehicle for suppressing their position. The Arian sympathizers experience it as exploitable doctrinal cover, allowing them to participate in official church structures while maintaining subordinationist theology. The engine computes these divergent directionalities from the structural data: agenda-setters have low d (beneficiary position), victims have high d (payers of coordination cost / suppression targets), excluded clergy have high d (trapped, forced to teach ambiguous doctrine). The claim (Tangled Rope) is structurally justified: the constraint coordinates (prevents schism, allows negotiated settlement) AND extracts (enforces conformity, suppresses heterodox readings, benefits the moderate faction and imperial authority disproportionately).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: (1) moderate_episcopal_faction — they author the formula, set the agenda, benefit from reputation as peacemakers, and gain institutional leverage over both extreme wings. (2) schism_avoiders / imperial_authority — they benefit from reduced schism risk and from the compromise's negotiated-settlement legitimacy. Victims: (1) strict_nicene_advocates — they lose doctrinal purity, see their victory reversed, must suppress and police Semi-Arian interpretations that blunt Nicene precision. (2) arian_sympathizers — they gain temporary cover but must publicly reject full Arianism and conform to an ambiguous formula they cannot fully endorse. Both victim groups pay the cost of enforced ambiguity and doctrinal theater. Directionality for the moderate episcopal faction approaches 0.15 (beneficiary, high power, arbitrage-level exit: they can always abandon the compromise). Directionality for strict Nicene advocates approaches 0.75 (high power but suppressed, constrained exit: their only options are conformity or schism). Directionality for imperial authority approaches 0.20 (benefits from settlement, but institutional responsibility for maintaining church order limits true exit). Directionality for parish clergy approaches 0.85 (powerless, trapped, forced to teach ambiguous doctrine they do not understand).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (schism risk, post-Nicaea doctrinal collapse) is stated as 'live' in the six-questions, but the measurement trajectory shows it degraded over the interval: extractiveness and theater peak mid-interval (345-355) and decline by 381, signaling the compromise's failure to maintain coordination or prevent mandatrophy. The Tangled Rope classification is defended because at 345-355 (the peak) the constraint BOTH coordinates (suppresses schism via negotiated formula) AND extracts (enforces conformity, suppresses heterodox readings, benefits the agenda-setter faction). By 381, when Pro-Nicene theology triumphs and the homoiousios formula is absorbed, the constraint loses its extractive function — it becomes a historical artifact. The measurement series capture this shift: theater and extraction both decline post-370, indicating the constraint's functional decomposition. If extractiveness had continued rising to 381, mandatrophy would be absent and the constraint would remain Tangled Rope or shift to Snare. The declining trend signals mandatrophy onset: the founding problem (preventing schism) is no longer solved by this constraint; Pro-Nicene dominance solves it differently, and the Semi-Arian formula becomes ceremonial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_vs_genuine_doctrine,
    'Is the homoiousios formula a genuine theological position, or is it primarily a tactical compromise designed to prevent schism?',
    'Textual analysis of Basil of Ancyra and George of Laodicea''s theological writings to distinguish whether they held homoiousios as substantively true or as diplomatically expedient. Comparison with their later retractions or affirmations when pressure changed.',
    'If primarily tactical, the constraint is a Snare (the formula is theater masking power struggle). If substantively held, it is a Tangled Rope (genuine theological coordination with asymmetric enforcement). The classification depends on the intention and conviction of the formula''s authors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compromise_vs_genuine_doctrine, empirical, 'Whether homoiousios was a sincere doctrine or a negotiation tactic.').

omega_variable(
    formula_interpretive_closure,
    'Did the homoiousios formula close interpretation (both factions agreed on what it meant) or did both sides continue to read it according to their own theology?',
    'Doctrinal analysis of Pro-Nicene and Arian bishops'' interpretations of homoiousios in post-345 conciliar statements and polemics. If both read it consistently one way, closure occurred. If they read it differently, it remained open.',
    'High interpretive closure would make the constraint Rope (genuine coordination on meaning achieved). Low closure would make it Snare (formula is theater; each faction maintains its own reading). The measurement-observed theater_ratio rising 0.22→0.49 suggests interpretive closure was never achieved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formula_interpretive_closure, empirical, 'Whether homoiousios achieved shared meaning or remained ambiguous.').

omega_variable(
    kernel_singularity_vs_factionalism,
    'Is there a single authoritative kernel (Nicene creed) that all three readings attempt to interpret, or are there three separate kernels (Pro-Nicene, Semi-Arian, Arian creedal statements) that the contest treats as fragments of one?',
    'Doctrinal historiography: trace which text (Nicaea 325, later synodal creeds, imperial rescripts) each faction treats as the authoritative kernel. If all cite the same creed and dispute its interpretation, kernel singularity holds. If they cite different creeds, the kernel is already fragmented.',
    'Kernel singularity supports the committer frame (one kernel, three readings). Fragmentation would suggest three separate constraints, not readings of one constraint. The historical record shows fragmentation accelerating: by 360s, different imperial authorities and councils issue different creeds (Sirmium creeds, Antioch creeds, semi-Arian creeds), suggesting the kernel splits into three by late interval.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_singularity_vs_factionalism, conceptual, 'Whether the homoousios_christology kernel remains singular or fragments into three.').

omega_variable(
    suppression_internalization_trajectory,
    'As the Semi-Arian constraint persists (345-370), do parish clergy and lay theologians internalize the homoiousios formula as correct, or does suppression remain purely structural (external coercion)?',
    'Post-381 evidence: when the Semi-Arian formula is absorbed into Pro-Nicene orthodoxy, do adherents accept the shift smoothly (internalization failed), or do they resist and maintain Semi-Arian positions (suppression was structural, not internalized)?',
    'High internalization by 381 would lower the effective suppression and shift the constraint toward Rope. Structural-only suppression would keep effective suppression high and maintain Tangled Rope / Snare classification. Historical evidence shows some smooth absorption (internalization occurred) and some active resistance (suppression was structural). The mixed outcome suggests suppression was partially internalized by some factions and remained structural for others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether suppression of Semi-Arian readings was internalized or purely external by 381.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the Pro-Nicene and Semi-Arian readings logically foreclose each other (one''s core premise rules out the other), or do they coexist as genuinely distinct positions held by different parties?',
    'Logical analysis of the foundational axioms: if Pro-Nicene ''homoousios means identical substance'' directly contradicts Semi-Arian ''homoiousios means similar (not identical) substance,'' then foreclosure holds; if they can be distinguished without logical contradiction, coexistence holds. Trace historical coexistence: did both readings persist simultaneously among different episcopal factions, or did one logically rule the other out?',
    'Foreclosure would make the reading_relations entry ''forecloses'' (rare); coexistence would make it ''coexists_with'' (common in theological disputes across factions). The historical record shows both readings coexisting 345-370, despite logical tension; this suggests coexistence (different factions hold incompatible positions without either ruling the other out within its own framework). At 381, Pro-Nicene victory forecloses Semi-Arian teaching within imperial orthodoxy, but does not logically eliminate the position — it is suppressed, not refuted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether readings logically foreclose or merely coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__semi_arian_reading, theater_ratio, 325, 0.22).
narrative_ontology:measurement(homo_tr_t335, homoousios_christology__semi_arian_reading, theater_ratio, 335, 0.31).
narrative_ontology:measurement(homo_tr_t345, homoousios_christology__semi_arian_reading, theater_ratio, 345, 0.41).
narrative_ontology:measurement(homo_tr_t355, homoousios_christology__semi_arian_reading, theater_ratio, 355, 0.49).
narrative_ontology:measurement(homo_tr_t370, homoousios_christology__semi_arian_reading, theater_ratio, 370, 0.44).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__semi_arian_reading, theater_ratio, 381, 0.42).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__semi_arian_reading, base_extractiveness, 325, 0.28).
narrative_ontology:measurement(homo_be_t335, homoousios_christology__semi_arian_reading, base_extractiveness, 335, 0.38).
narrative_ontology:measurement(homo_be_t345, homoousios_christology__semi_arian_reading, base_extractiveness, 345, 0.45).
narrative_ontology:measurement(homo_be_t355, homoousios_christology__semi_arian_reading, base_extractiveness, 355, 0.51).
narrative_ontology:measurement(homo_be_t370, homoousios_christology__semi_arian_reading, base_extractiveness, 370, 0.42).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__semi_arian_reading, base_extractiveness, 381, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__semi_arian_reading, suppression_requirement, 325, 0.18).
narrative_ontology:measurement(homo_su_t335, homoousios_christology__semi_arian_reading, suppression_requirement, 335, 0.28).
narrative_ontology:measurement(homo_su_t345, homoousios_christology__semi_arian_reading, suppression_requirement, 345, 0.38).
narrative_ontology:measurement(homo_su_t355, homoousios_christology__semi_arian_reading, suppression_requirement, 355, 0.42).
narrative_ontology:measurement(homo_su_t370, homoousios_christology__semi_arian_reading, suppression_requirement, 370, 0.37).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__semi_arian_reading, suppression_requirement, 381, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three structurally distinct constraint stories, one per reading. The Semi-Arian reading (this story) coordinates around identity (episcopal group membership and orthodox belief) while managing extraction (asymmetric enforcement favoring moderates). The Pro-Nicene reading instantiates higher extractiveness (victory consolidation, retrospective suppression of Semi-Arianism). The Arian reading instantiates suppression and victim positioning from the outset. All three share the same referent (the standing christological arrangement post-325) but differ in ε (the standing arrangement assessed by each reading's own lights). The network edges model contamination: Semi-Arian absorption into Pro-Nicene (influences edge) means Pro-Nicene success directly undermines Semi-Arian legitimacy and eliminates the third reading's functional viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
